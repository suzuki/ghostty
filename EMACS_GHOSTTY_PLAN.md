# emacs-ghostty: Implementation Plan (Approach A)

libghostty-vt の Zig 内部 API 上に Zig→C ブリッジを構築し、
Emacs dynamic module として emacs-libvterm 相当の機能を実現する。

**最小要求バージョン: Emacs 30**
— 新しい Elisp 機能・マクロ (`defvar-keymap`, `keymap-set`, `pcase-let`,
`when-let*`, `pos-bol`/`pos-eol`, `use-package` 組み込み等) を積極的に活用する。

---

## 1. アーキテクチャ概要

```
┌──────────────────────────────────────────────┐
│  Emacs (Elisp)                               │
│  ghostty-term.el                            │
│  - make-process (:connection-type 'pty)      │
│  - process filter → C module へバイト列転送  │
│  - timer-coalesced redraw (10ms)             │
│  - テキストプロパティで色・属性を表現        │
└──────────────────┬───────────────────────────┘
                   │ Emacs Dynamic Module API
                   │ (emacs-module.h)
┌──────────────────▼───────────────────────────┐
│  ghostty-term-module.c                      │
│  - emacs_module_init() エントリポイント      │
│  - Emacs funcall でバッファ操作              │
│  - user_ptr で GhosttyTerm* を保持           │
│  - キー入力 → Zig ブリッジへ転送            │
│  - 画面状態 → Emacs バッファへ描画           │
└──────────────────┬───────────────────────────┘
                   │ C ABI (ghostty_vt_bridge.h)
┌──────────────────▼───────────────────────────┐
│  ghostty_vt_bridge.zig                       │
│  - Terminal.init/deinit ラッパー             │
│  - vtStream().process() ラッパー             │
│  - スクリーン状態読取 API                    │
│  - セル・属性・カーソル情報の C 構造体変換   │
│  - リサイズ・スクロールバック・選択          │
└──────────────────┬───────────────────────────┘
                   │ Zig internal API
┌──────────────────▼───────────────────────────┐
│  libghostty-vt (Zig)                         │
│  - terminal/Terminal.zig                     │
│  - terminal/Screen.zig                       │
│  - terminal/page.zig (Cell, Row, Style)      │
│  - terminal/PageList.zig (スクロールバック)  │
└──────────────────────────────────────────────┘
```

---

## 2. レイヤー別 詳細設計

### 2.1 Layer 1: Zig→C ブリッジ (`ghostty_vt_bridge.zig`)

libghostty-vt の Zig 内部 API をラップし、C ABI で公開する。
Emacs module (C) からはこのレイヤーのみを呼ぶ。

#### 2.1.1 不透明ハンドル

```c
// ghostty_vt_bridge.h
typedef struct GhosttyTerm GhosttyTerm;
```

Zig 側で `Terminal` + `Allocator` + mutex + ストリーム状態を保持する構造体。

```zig
// ghostty_vt_bridge.zig
const GhosttyTerm = struct {
    alloc: std.mem.Allocator,
    terminal: Terminal,
    mutex: std.Thread.Mutex,
    // ダーティトラッキング用のメタデータ
    title: ?[]const u8,
    title_changed: bool,
    bell: bool,
    pwd: ?[]const u8,
    pwd_changed: bool,
};
```

#### 2.1.2 エクスポート関数一覧

**ライフサイクル:**

```c
// ターミナル生成 (cols, rows, max_scrollback)
GhosttyTerm* ghostty_vt_new(uint16_t cols, uint16_t rows, uint32_t max_scrollback);

// ターミナル破棄
void ghostty_vt_free(GhosttyTerm* term);
```

**入力処理:**

```c
// PTY 出力バイト列を処理 (VT パース → 内部状態更新)
// 戻り値: 0=成功, -1=エラー
int ghostty_vt_process(GhosttyTerm* term, const uint8_t* data, size_t len);
```

**画面状態読取:**

```c
// ターミナルサイズ取得
void ghostty_vt_get_size(const GhosttyTerm* term, uint16_t* cols, uint16_t* rows);

// カーソル位置・スタイル取得
typedef struct {
    uint16_t x;
    uint16_t y;
    uint8_t  style;  // 0=block, 1=bar, 2=underline
    bool     visible;
} GhosttyVtCursor;

void ghostty_vt_get_cursor(const GhosttyTerm* term, GhosttyVtCursor* cursor);

// セル情報取得 (1セル分)
typedef struct {
    uint32_t codepoint;        // 最初の Unicode コードポイント
    uint8_t  extra_cp_count;   // 追加コードポイント数 (合成文字)
    uint32_t extra_cps[8];     // 追加コードポイント (最大8)
    uint8_t  width;            // 表示幅 (1 or 2)
    // 色 (RGB, 各0なら未指定 → デフォルト色)
    bool     fg_set;
    uint8_t  fg_r, fg_g, fg_b;
    bool     bg_set;
    uint8_t  bg_r, bg_g, bg_b;
    bool     ul_set;           // 下線色
    uint8_t  ul_r, ul_g, ul_b;
    // 属性フラグ
    bool     bold;
    bool     italic;
    bool     underline;
    uint8_t  underline_style;  // 0=none,1=single,2=double,3=curly,4=dotted,5=dashed
    bool     strikethrough;
    bool     inverse;
    bool     faint;
    bool     blink;
    bool     overline;
    bool     invisible;
    // ハイパーリンク
    bool     has_hyperlink;
    // ワイド文字のスペーサーかどうか
    bool     is_spacer;
} GhosttyVtCell;

// 特定座標のセル情報取得
// 戻り値: 0=成功, -1=範囲外
int ghostty_vt_get_cell(const GhosttyTerm* term, uint16_t x, uint16_t y,
                        GhosttyVtCell* cell);

// 行全体のテキストを UTF-8 で取得 (バッファサイズは cols*4+1 程度確保)
// 戻り値: 書き込まれたバイト数, -1=エラー
int ghostty_vt_get_row_text(const GhosttyTerm* term, uint16_t y,
                            char* buf, size_t buf_size);
```

**ダーティ追跡 (差分描画):**

```c
// ダーティ状態の問い合わせ
typedef struct {
    bool     screen_cleared;  // 全画面再描画必要
    bool     palette_changed; // カラーパレット変更
    bool     title_changed;   // ウィンドウタイトル変更
    bool     bell;            // ベル発生
    bool     pwd_changed;     // カレントディレクトリ変更
    uint16_t dirty_row_start; // ダーティ行範囲 (start <= row < end)
    uint16_t dirty_row_end;
} GhosttyVtDirtyState;

// ダーティ状態を取得し、内部フラグをクリア
void ghostty_vt_get_dirty(GhosttyTerm* term, GhosttyVtDirtyState* state);

// 特定行がダーティかチェック
bool ghostty_vt_row_is_dirty(const GhosttyTerm* term, uint16_t y);

// 全ダーティフラグをクリア
void ghostty_vt_clear_dirty(GhosttyTerm* term);
```

**リサイズ:**

```c
// ターミナルリサイズ (リフロー含む)
// 戻り値: 0=成功, -1=エラー
int ghostty_vt_resize(GhosttyTerm* term, uint16_t cols, uint16_t rows);
```

**スクロールバック:**

```c
// スクロールバック行数取得
uint32_t ghostty_vt_scrollback_lines(const GhosttyTerm* term);

// スクロールバック行のセル取得 (y=0 が最も古い行)
int ghostty_vt_get_scrollback_cell(const GhosttyTerm* term, uint32_t y,
                                   uint16_t x, GhosttyVtCell* cell);

// スクロールバック行のテキスト取得
int ghostty_vt_get_scrollback_row_text(const GhosttyTerm* term, uint32_t y,
                                       char* buf, size_t buf_size);
```

**カラーパレット:**

```c
// パレット色取得 (index: 0-255)
void ghostty_vt_get_palette_color(const GhosttyTerm* term, uint8_t index,
                                  uint8_t* r, uint8_t* g, uint8_t* b);

// デフォルト前景/背景色取得
void ghostty_vt_get_default_fg(const GhosttyTerm* term,
                               uint8_t* r, uint8_t* g, uint8_t* b);
void ghostty_vt_get_default_bg(const GhosttyTerm* term,
                               uint8_t* r, uint8_t* g, uint8_t* b);
```

**ターミナルモード:**

```c
// 現在のスクリーンが alternate か
bool ghostty_vt_is_alternate_screen(const GhosttyTerm* term);

// アプリケーションカーソルキーモードか
bool ghostty_vt_is_app_cursor(const GhosttyTerm* term);

// アプリケーションキーパッドモードか
bool ghostty_vt_is_app_keypad(const GhosttyTerm* term);

// マウスモード取得
uint8_t ghostty_vt_mouse_mode(const GhosttyTerm* term);

// ブラケットペーストモードか
bool ghostty_vt_is_bracketed_paste(const GhosttyTerm* term);
```

**メタデータ:**

```c
// タイトル取得 (OSC 0/2)
const char* ghostty_vt_get_title(const GhosttyTerm* term);

// カレントディレクトリ取得 (OSC 7)
const char* ghostty_vt_get_pwd(const GhosttyTerm* term);
```

**行の効率的一括読取 (描画パフォーマンス最適化):**

```c
// 行の全セルを一括取得 (バッチ API)
// cells 配列は cols 個以上確保すること
// 戻り値: 実際のセル数
int ghostty_vt_get_row_cells(const GhosttyTerm* term, uint16_t y,
                             GhosttyVtCell* cells, uint16_t max_cells);

// 行が wrap (ソフト改行) かどうか
bool ghostty_vt_row_is_wrapped(const GhosttyTerm* term, uint16_t y);
```

#### 2.1.3 Zig 実装のポイント

```zig
// ghostty_vt_bridge.zig

const std = @import("std");
const Terminal = @import("terminal/Terminal.zig");
const pagepkg = @import("terminal/page.zig");
const stylepkg = @import("terminal/style.zig");
const color = @import("terminal/color.zig");

export fn ghostty_vt_new(
    cols: u16,
    rows: u16,
    max_scrollback: u32,
) ?*GhosttyTerm {
    const alloc = std.heap.c_allocator;  // libc malloc ベース
    const term = alloc.create(GhosttyTerm) catch return null;
    term.* = .{
        .alloc = alloc,
        .terminal = Terminal.init(alloc, .{
            .cols = cols,
            .rows = rows,
            .max_scrollback = max_scrollback,
        }) catch {
            alloc.destroy(term);
            return null;
        },
        .mutex = .{},
        .title = null,
        .title_changed = false,
        .bell = false,
        .pwd = null,
        .pwd_changed = false,
    };
    return term;
}

export fn ghostty_vt_process(
    term: *GhosttyTerm,
    data: [*]const u8,
    len: usize,
) c_int {
    term.mutex.lock();
    defer term.mutex.unlock();

    var stream = term.terminal.vtStream();
    defer stream.deinit();
    stream.process(data[0..len]) catch return -1;
    return 0;
}

export fn ghostty_vt_get_cell(
    term: *const GhosttyTerm,
    x: u16,
    y: u16,
    out: *GhosttyVtCell,
) c_int {
    const screen = term.terminal.screens.active;
    if (x >= term.terminal.cols or y >= term.terminal.rows) return -1;

    const page = screen.pages.pages.first.?.data;
    const rac = page.getRowAndCell(x, y);
    const cell = rac.cell;

    out.* = .{};  // ゼロ初期化
    out.is_spacer = (cell.wide == .spacer_tail or cell.wide == .spacer_head);
    out.width = cell.gridWidth();
    out.codepoint = cell.codepoint();

    // グラフェムクラスタ処理
    if (cell.hasGrapheme()) {
        if (page.lookupGrapheme(cell)) |extras| {
            out.extra_cp_count = @intCast(@min(extras.len, 8));
            for (extras[0..out.extra_cp_count], 0..) |cp, i| {
                out.extra_cps[i] = cp;
            }
        }
    }

    // スタイル解決
    const style = if (cell.style_id == 0)
        stylepkg.Style{}
    else
        page.styles.get(page.memory, cell.style_id).*;

    // フラグ
    out.bold = style.flags.bold;
    out.italic = style.flags.italic;
    out.faint = style.flags.faint;
    out.strikethrough = style.flags.strikethrough;
    out.inverse = style.flags.inverse;
    out.blink = style.flags.blink;
    out.overline = style.flags.overline;
    out.invisible = style.flags.invisible;
    out.underline = style.flags.underline != .none;
    out.underline_style = @intFromEnum(style.flags.underline);

    // 色解決
    resolve_color(style.fg_color, &out.fg_set, &out.fg_r, &out.fg_g, &out.fg_b,
                  &term.terminal);
    resolve_color(style.bg_color, &out.bg_set, &out.bg_r, &out.bg_g, &out.bg_b,
                  &term.terminal);
    resolve_color(style.underline_color, &out.ul_set, &out.ul_r, &out.ul_g, &out.ul_b,
                  &term.terminal);

    return 0;
}

fn resolve_color(
    c: stylepkg.Color,
    set: *bool,
    r: *u8,
    g: *u8,
    b: *u8,
    terminal: *const Terminal,
) void {
    switch (c) {
        .none => { set.* = false; },
        .palette => |idx| {
            const rgb = terminal.colors.palette.current[idx];
            set.* = true;
            r.* = rgb.r;
            g.* = rgb.g;
            b.* = rgb.b;
        },
        .rgb => |rgb| {
            set.* = true;
            r.* = rgb.r;
            g.* = rgb.g;
            b.* = rgb.b;
        },
    }
}
```

#### 2.1.4 ビルド設定

```zig
// build.zig に追加 (もしくは独立した build.zig)
const bridge = b.addSharedLibrary(.{
    .name = "ghostty-vt-bridge",
    .root_source_file = b.path("src/ghostty_vt_bridge.zig"),
    .target = target,
    .optimize = optimize,
});
bridge.linkLibC();
b.installArtifact(bridge);
```

---

### 2.2 Layer 2: Emacs C モジュール (`ghostty-term-module.c`)

Emacs dynamic module API を使い、Zig ブリッジを Emacs から操作する。

#### 2.2.1 ファイル構成

```
emacs-ghostty/
├── ghostty-term-module.c    # Emacs dynamic module (C)
├── ghostty-term-module.h    # 共有型定義
├── ghostty-term.el          # Elisp フロントエンド
├── ghostty_vt_bridge.h       # Zig ブリッジのヘッダ
└── build.zig                 # ビルドスクリプト
    (or CMakeLists.txt/Makefile)
```

#### 2.2.2 C モジュール主要関数

```c
// ghostty-term-module.c
#include <emacs-module.h>
#include "ghostty_vt_bridge.h"

int plugin_is_GPL_compatible;

// ============================================================
// 内部状態
// ============================================================
typedef struct {
    GhosttyTerm *term;
    emacs_value  title_callback;   // OSC タイトル変更通知
    emacs_value  bell_callback;    // ベル通知
    emacs_value  pwd_callback;     // ディレクトリ変更通知
} EmacsGhosttyTerm;

// GC ファイナライザ
static void term_finalizer(void *ptr) {
    EmacsGhosttyTerm *egt = ptr;
    if (egt->term) {
        ghostty_vt_free(egt->term);
        egt->term = NULL;
    }
    free(egt);
}

// ============================================================
// Emacs 関数: ghostty-term--new
// ============================================================
static emacs_value
Fghostty_term_new(emacs_env *env, ptrdiff_t nargs,
                   emacs_value args[], void *data)
{
    intmax_t cols = env->extract_integer(env, args[0]);
    intmax_t rows = env->extract_integer(env, args[1]);
    intmax_t scrollback = (nargs > 2)
        ? env->extract_integer(env, args[2])
        : 10000;

    EmacsGhosttyTerm *egt = calloc(1, sizeof(*egt));
    egt->term = ghostty_vt_new(cols, rows, scrollback);
    if (!egt->term) {
        free(egt);
        env->non_local_exit_signal(env,
            env->intern(env, "error"),
            env->make_string(env, "Failed to create terminal", 25));
        return env->intern(env, "nil");
    }
    return env->make_user_ptr(env, term_finalizer, egt);
}

// ============================================================
// Emacs 関数: ghostty-term--write-input
// ============================================================
static emacs_value
Fghostty_term_write_input(emacs_env *env, ptrdiff_t nargs,
                           emacs_value args[], void *data)
{
    EmacsGhosttyTerm *egt = env->get_user_ptr(env, args[0]);
    ptrdiff_t len = 0;
    env->copy_string_contents(env, args[1], NULL, &len);
    char *buf = malloc(len);
    env->copy_string_contents(env, args[1], buf, &len);

    int rc = ghostty_vt_process(egt->term, (uint8_t *)buf, len - 1);
    free(buf);

    return env->make_integer(env, rc);
}

// ============================================================
// Emacs 関数: ghostty-term--redraw
// (ダーティ行のみバッファを更新)
// ============================================================
static emacs_value
Fghostty_term_redraw(emacs_env *env, ptrdiff_t nargs,
                      emacs_value args[], void *data)
{
    EmacsGhosttyTerm *egt = env->get_user_ptr(env, args[0]);
    GhosttyVtDirtyState dirty;
    ghostty_vt_get_dirty(egt->term, &dirty);

    if (!dirty.screen_cleared &&
        dirty.dirty_row_start >= dirty.dirty_row_end) {
        // 変更なし
        return env->intern(env, "nil");
    }

    uint16_t cols, rows;
    ghostty_vt_get_size(egt->term, &cols, &rows);

    uint16_t start = dirty.screen_cleared ? 0 : dirty.dirty_row_start;
    uint16_t end   = dirty.screen_cleared ? rows : dirty.dirty_row_end;

    // Elisp: (ghostty-term--refresh-lines term start end)
    // 各行のセルを読み取り、テキストプロパティ付きでバッファに挿入
    for (uint16_t y = start; y < end; y++) {
        refresh_line(env, egt, y, cols);
        if (env->should_quit(env))
            break;
    }

    // カーソル位置更新
    GhosttyVtCursor cursor;
    ghostty_vt_get_cursor(egt->term, &cursor);
    emacs_value goto_args[] = {
        env->make_integer(env, cursor_to_point(cursor, cols))
    };
    env->funcall(env, env->intern(env, "goto-char"), 1, goto_args);

    // メタデータコールバック
    if (dirty.title_changed) {
        const char *title = ghostty_vt_get_title(egt->term);
        if (title) {
            emacs_value title_val = env->make_string(env, title, strlen(title));
            emacs_value cb_args[] = { title_val };
            env->funcall(env, env->intern(env, "ghostty-term--set-title"),
                        1, cb_args);
        }
    }

    if (dirty.bell) {
        env->funcall(env, env->intern(env, "ding"), 0, NULL);
    }

    ghostty_vt_clear_dirty(egt->term);
    return env->intern(env, "t");
}

// ============================================================
// refresh_line: 1行分をバッファに描画
// ============================================================
static void refresh_line(emacs_env *env, EmacsGhosttyTerm *egt,
                         uint16_t y, uint16_t cols)
{
    GhosttyVtCell cells[cols];
    int n = ghostty_vt_get_row_cells(egt->term, y, cells, cols);

    // UTF-8 テキストを組み立て
    char text_buf[cols * 4 + 1];
    int text_len = 0;

    for (int x = 0; x < n; x++) {
        if (cells[x].is_spacer) continue;

        // codepoint → UTF-8
        text_len += encode_utf8(cells[x].codepoint,
                                text_buf + text_len);
        for (int j = 0; j < cells[x].extra_cp_count; j++) {
            text_len += encode_utf8(cells[x].extra_cps[j],
                                    text_buf + text_len);
        }
    }
    text_buf[text_len] = '\0';

    // バッファの該当行を置換
    // (goto-line y+1) → (delete-region (line-beginning) (line-end)) → (insert text)
    // テキストプロパティで face を設定
    // ...
}

// ============================================================
// モジュール初期化
// ============================================================
/* Helper: bind a C function to a Lisp symbol via defalias */
static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
    emacs_value Qdefalias = env->intern(env, "defalias");
    emacs_value Qsym = env->intern(env, name);
    emacs_value args[] = { Qsym, Sfun };
    env->funcall(env, Qdefalias, 2, args);
}

/* Helper: provide a feature */
static void provide(emacs_env *env, const char *feature) {
    emacs_value Qfeat = env->intern(env, feature);
    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value args[] = { Qfeat };
    env->funcall(env, Qprovide, 1, args);
}

int emacs_module_init(struct emacs_runtime *ert) {
    if (ert->size < sizeof(*ert)) return 1;
    emacs_env *env = ert->get_environment(ert);
    if (env->size < sizeof(*env)) return 2;

    #define DEFUN(lsym, csym, min, max, doc, data) \
        bind_function(env, lsym, \
            env->make_function(env, min, max, csym, doc, data))

    DEFUN("ghostty-term--new",         Fghostty_term_new,         2, 3,
          "Create a new ghostty terminal (COLS ROWS &optional SCROLLBACK).",
          NULL);
    DEFUN("ghostty-term--write-input", Fghostty_term_write_input, 2, 2,
          "Write input bytes to terminal.", NULL);
    DEFUN("ghostty-term--redraw",      Fghostty_term_redraw,      1, 1,
          "Redraw dirty lines into Emacs buffer.", NULL);
    DEFUN("ghostty-term--resize",      Fghostty_term_resize,      3, 3,
          "Resize terminal (TERM COLS ROWS).", NULL);
    DEFUN("ghostty-term--get-cursor",  Fghostty_term_get_cursor,  1, 1,
          "Get cursor position as (X . Y).", NULL);
    // ... 他の関数も同様

    #undef DEFUN

    provide(env, "ghostty-term-module");
    return 0;
}
```

#### 2.2.3 描画戦略

emacs-libvterm と同じ方式を採用:

1. **Emacs が PTY を所有**: `make-process` で `:connection-type 'pty` 指定
2. **プロセスフィルタ**で PTY 出力を受信 → `ghostty-term--write-input` で Zig ブリッジへ転送
3. **タイマー合体描画**: 10ms タイマーで `ghostty-term--redraw` を呼ぶ
   - 即座のインタラクティブ応答が必要な場合はタイマーをバイパス
4. **差分描画**: `ghostty_vt_get_dirty()` でダーティ行のみ更新
5. **テキストプロパティ**: `font-lock-face` で色・属性を表現

```
PTY 出力
  → Emacs event loop (select/poll)
  → ghostty-term--filter (Elisp)
    → ghostty-term--write-input (C module → Zig bridge)
      → Terminal.vtStream().process()
      → ダーティフラグ更新
  → タイマー発火 (10ms)
    → ghostty-term--redraw (C module)
      → ghostty_vt_get_dirty() でダーティ行特定
      → ghostty_vt_get_row_cells() でセル読取
      → Emacs バッファに insert + text-property 設定
```

---

### 2.3 Layer 3: Elisp フロントエンド (`ghostty-term.el`)

#### 2.3.1 主要コンポーネント

```elisp
;;; ghostty-term.el --- Terminal emulator powered by libghostty -*- lexical-binding: t -*-

;; Requires: Emacs 30+

(require 'ghostty-term-module)

;; ============================================================
;; カスタマイズ変数
;; ============================================================
(defgroup ghostty-term nil
  "Terminal emulator powered by libghostty."
  :group 'terminals)

(defcustom ghostty-term-shell shell-file-name
  "Shell to run in the terminal."
  :type 'string)

(defcustom ghostty-term-max-scrollback 10000
  "Maximum scrollback lines."
  :type 'natnum)

(defcustom ghostty-term-timer-delay 0.01
  "Delay for coalescing redraws (seconds)."
  :type 'float)

(defcustom ghostty-term-kill-buffer-on-exit t
  "Kill the terminal buffer when the shell process exits."
  :type 'boolean)

;; ============================================================
;; キーマップ (Emacs 29+: defvar-keymap)
;; ============================================================
(defvar-keymap ghostty-term-mode-map
  :doc "Keymap for `ghostty-term-mode'."
  :parent special-mode-map
  ;; 全印字可能文字 + 制御キーをターミナルに転送
  "RET"       #'ghostty-term--send-key
  "TAB"       #'ghostty-term--send-key
  "DEL"       #'ghostty-term--send-key
  "SPC"       #'ghostty-term--send-key
  "<up>"      #'ghostty-term--send-key
  "<down>"    #'ghostty-term--send-key
  "<left>"    #'ghostty-term--send-key
  "<right>"   #'ghostty-term--send-key
  "<home>"    #'ghostty-term--send-key
  "<end>"     #'ghostty-term--send-key
  "C-c C-c"   #'ghostty-term--send-ctrl-c
  "C-c C-z"   #'ghostty-term--send-ctrl-z
  "C-c C-\\"  #'ghostty-term--send-ctrl-backslash)

;; 印字可能文字の一括バインド (Emacs 29+: keymap-set)
(dotimes (i 95)
  (keymap-set ghostty-term-mode-map
              (string (+ i 32))
              #'ghostty-term--self-insert))

;; ============================================================
;; メジャーモード
;; ============================================================
(define-derived-mode ghostty-term-mode special-mode "GhosttyTerm"
  "Major mode for Ghostty terminal emulator.

Requires Emacs 30 or later."
  :group 'ghostty-term
  :interactive nil
  (setq-local buffer-read-only t)
  (setq-local ghostty-term--term nil)
  (setq-local ghostty-term--process nil)
  (setq-local ghostty-term--redraw-timer nil)
  (setq-local scroll-conservatively 101)
  (setq-local cursor-type 'box)
  (setq-local truncate-lines t)
  ;; Emacs 29+: window-size-change-functions はバッファローカルにできる
  (add-hook 'window-size-change-functions
            #'ghostty-term--window-size-change nil t))

;; ============================================================
;; エントリポイント
;; ============================================================
;;;###autoload
(defun ghostty-term ()
  "Create a new Ghostty terminal buffer."
  (interactive)
  (unless (>= emacs-major-version 30)
    (user-error "ghostty-term requires Emacs 30 or later"))
  (let ((buf (generate-new-buffer "*ghostty-term*")))
    (with-current-buffer buf
      (ghostty-term-mode)
      (pcase-let ((`(,cols . ,rows) (ghostty-term--window-size)))
        (setq ghostty-term--term
              (ghostty-term--new cols rows ghostty-term-max-scrollback))
        (setq ghostty-term--process
              (make-process
               :name "ghostty-term"
               :buffer buf
               :command `("/bin/sh" "-c"
                         ,(format "stty sane erase ^? rows %d columns %d && exec %s"
                                  rows cols ghostty-term-shell))
               :connection-type 'pty
               :filter #'ghostty-term--filter
               :sentinel #'ghostty-term--sentinel))))
    (pop-to-buffer buf)))

;; ============================================================
;; プロセスフィルタ (PTY → ターミナル)
;; ============================================================
(defun ghostty-term--filter (process output)
  "Process filter: feed PTY output to ghostty terminal."
  (when-let* ((buf (process-buffer process))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (ghostty-term--write-input ghostty-term--term output)
      (ghostty-term--schedule-redraw))))

;; ============================================================
;; プロセスセンチネル
;; ============================================================
(defun ghostty-term--sentinel (process event)
  "Handle shell process state changes."
  (when-let* ((buf (process-buffer process))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (when ghostty-term--redraw-timer
        (cancel-timer ghostty-term--redraw-timer)
        (setq ghostty-term--redraw-timer nil))
      (run-hook-with-args 'ghostty-term-exit-functions buf event)
      (when (and ghostty-term-kill-buffer-on-exit
                 (memq (process-status process) '(exit signal)))
        (kill-buffer buf)))))

(defvar ghostty-term-exit-functions nil
  "Abnormal hook run when the terminal process exits.
Each function receives the buffer and event string.")

;; ============================================================
;; タイマー合体描画
;; ============================================================
(defun ghostty-term--schedule-redraw ()
  "Schedule a coalesced redraw."
  (unless ghostty-term--redraw-timer
    (setq ghostty-term--redraw-timer
          (run-with-timer ghostty-term-timer-delay nil
                          #'ghostty-term--do-redraw
                          (current-buffer)))))

(defun ghostty-term--do-redraw (buf)
  "Perform the actual redraw."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq ghostty-term--redraw-timer nil)
      (let ((inhibit-read-only t))
        (ghostty-term--redraw ghostty-term--term)))))

;; ============================================================
;; キー入力
;; ============================================================
(defun ghostty-term--self-insert ()
  "Send the current key event to the terminal as a character."
  (interactive)
  (when-let* ((keys (this-command-keys-vector))
              (encoded (ghostty-term--encode-key keys)))
    (process-send-string ghostty-term--process encoded)))

(defun ghostty-term--send-key ()
  "Send the current key event to the terminal."
  (interactive)
  (when-let* ((keys (this-command-keys-vector))
              (encoded (ghostty-term--encode-key keys)))
    (process-send-string ghostty-term--process encoded)))

(defun ghostty-term--send-ctrl-c ()
  "Send C-c to the terminal."
  (interactive)
  (process-send-string ghostty-term--process "\C-c"))

(defun ghostty-term--send-ctrl-z ()
  "Send C-z to the terminal."
  (interactive)
  (process-send-string ghostty-term--process "\C-z"))

(defun ghostty-term--send-ctrl-backslash ()
  "Send C-\\ to the terminal."
  (interactive)
  (process-send-string ghostty-term--process "\C-\\"))

;; ============================================================
;; リサイズ
;; ============================================================
(defun ghostty-term--window-size-change (_frame)
  "Handle window resize."
  ;; ghostty-term-mode のバッファローカルフックとして登録されるため、
  ;; current-buffer は既に正しいバッファを指す
  (when (and ghostty-term--term ghostty-term--process)
    (pcase-let ((`(,cols . ,rows) (ghostty-term--window-size)))
      (ghostty-term--resize ghostty-term--term cols rows)
      (set-process-window-size ghostty-term--process rows cols)
      (ghostty-term--schedule-redraw))))

;; ============================================================
;; use-package サポート (Emacs 29+ 組み込み)
;; ============================================================
;; (use-package ghostty-term
;;   :commands ghostty-term
;;   :custom
;;   (ghostty-term-shell "/bin/zsh")
;;   (ghostty-term-max-scrollback 20000))

(provide 'ghostty-term)
;;; ghostty-term.el ends here
```

---

## 3. 実装フェーズ

### Phase 1: 基本ブリッジ + 最小動作 (MVP)

**目標**: テキスト表示とキー入力が動く最小限

| # | タスク | 詳細 |
|---|--------|------|
| 1.1 | Zig ブリッジ骨格 | `ghostty_vt_new/free/process/get_size/get_cursor/get_cell/get_row_cells` |
| 1.2 | ビルドシステム | 独立した `build.zig` で `libghostty-vt-bridge.so` を生成 |
| 1.3 | C モジュール骨格 | `emacs_module_init`, `new`, `write-input`, `redraw`, `resize` |
| 1.4 | Elisp 最小フレーム | メジャーモード、プロセス起動、フィルタ、描画ループ |
| 1.5 | 統合テスト | `ls`, `echo`, 基本的なシェル操作の確認 |

**成果物**: モノクロのテキスト表示 + 基本キー入力

### Phase 2: 色・属性・差分描画

| # | タスク | 詳細 |
|---|--------|------|
| 2.1 | ダーティ追跡実装 | `ghostty_vt_get_dirty/clear_dirty/row_is_dirty` |
| 2.2 | 色マッピング | RGB → Emacs face 変換 (256色 + TrueColor) |
| 2.3 | 属性サポート | bold, italic, underline, strikethrough → face |
| 2.4 | 差分描画 | ダーティ行のみ再描画 |
| 2.5 | タイマー合体 | 高スループット時のバッチ描画 |

**成果物**: カラー表示 + 高速描画

### Phase 3: 高度な機能

| # | タスク | 詳細 |
|---|--------|------|
| 3.1 | スクロールバック | 履歴閲覧 (Emacs のスクロール連携) |
| 3.2 | マウスサポート | マウスイベント転送 (xterm mouse protocol) |
| 3.3 | 選択・コピー | Emacs のリージョン ↔ ターミナル選択 |
| 3.4 | OSC メタデータ | タイトル、ディレクトリ追跡 |
| 3.5 | ブラケットペースト | ペーストモード対応 |
| 3.6 | Alternate screen | vim/less 等のフルスクリーンアプリ |

### Phase 4: 最適化・安定化

| # | タスク | 詳細 |
|---|--------|------|
| 4.1 | パフォーマンス計測 | プロファイリング (大量出力、リサイズ) |
| 4.2 | 日本語・CJK | ワイド文字、合成文字の完全対応 |
| 4.3 | ハイパーリンク | OSC 8 ハイパーリンクサポート |
| 4.4 | Shell integration | OSC 133 (プロンプト検出) |
| 4.5 | Kitty keyboard | 拡張キーボードプロトコル |

---

## 4. 技術的リスクと対策

### リスク 1: Zig 内部 API の不安定性

**リスク**: Terminal.zig の内部構造がアップストリームの更新で変わる。

**対策**:
- ブリッジ層を薄くし、変更箇所を最小化
- Ghostty の特定タグ/コミットに固定して開発
- 将来的に公式 C API が拡充されたら移行

### リスク 2: ビルドの複雑さ

**リスク**: Zig + C のクロスビルドが Emacs パッケージとして配布困難。

**対策**:
- プリビルドバイナリの提供 (GitHub Releases)
- Nix flake での再現可能ビルド
- MELPA 配布時は `vterm` と同様にビルドステップを提供

### リスク 3: Emacs スレッドモデルとの衝突

**リスク**: Emacs はシングルスレッド。Zig ブリッジ内でのロックが問題になる可能性。

**対策**:
- emacs-libvterm と同じ設計を採用 (Emacs がすべてを駆動)
- Zig ブリッジ内でバックグラウンドスレッドは起動しない
- mutex は将来的な拡張 (非同期レンダリング) のためだけに用意

### リスク 4: libghostty-vt のコールバック不足

**リスク**: Terminal.zig の VT ストリームハンドラが、bell/title/pwd 等の
イベントを外部に通知する仕組みがない (内部状態を更新するだけ)。

**対策**:
- ストリーム処理後に状態をポーリングして変化を検出
- `terminal.flags.dirty` + 独自の前回値比較でイベントを擬似生成
- 必要に応じて Terminal.zig にフック用フィールドを追加 (fork 内)

### リスク 5: ページリスト跨ぎのスクロールバック

**リスク**: スクロールバックが複数ページにまたがる場合、
直接 `page.getRow()` では最初のページしかアクセスできない。

**対策**:
- `screen.pages.pin()` + `Point.screen` 座標系でアクセス
- ページイテレータ (`pageIterator`) を使用
- ブリッジ API では y=0 をスクロールバック先頭として抽象化

---

## 5. emacs-libvterm との機能対応表

| emacs-libvterm 機能 | ghostty-term 対応方針 |
|---------------------|----------------------|
| libvterm の VTerm/VTermScreen | Terminal.zig + Screen.zig |
| vterm_input_write() | Terminal.vtStream().process() |
| VTermScreenCell の読取 | Page.getRowAndCell() + Style 解決 |
| damage callback | ダーティフラグ (row.dirty, page.dirty, terminal.flags.dirty) |
| vterm_screen_get_cell() | ghostty_vt_get_cell() |
| vterm_state_set_callbacks() | ポーリング方式 (フラグ差分検出) |
| process filter | 同一方式 (make-process + filter) |
| timer-coalesced redraw | 同一方式 (run-with-timer 0.01s) |
| scrollback ring buffer | PageList (リンクリスト方式) |
| Sixel/iTerm2 画像 | Terminal.zig が内部サポート (将来対応) |
| ディレクトリ追跡 (OSC 7) | terminal.pwd フィールド |
| タイトル (OSC 0/2) | terminal.title フィールド |
| Elisp eval (OSC 51;E) | カスタム OSC ハンドラ追加 |

---

## 6. libghostty-vt を選ぶメリット (vs libvterm)

| 観点 | libvterm | libghostty-vt |
|------|----------|---------------|
| VT パース品質 | 良好 | 優秀 (Ghostty で実証済み) |
| Kitty keyboard protocol | 未対応 | フルサポート |
| Kitty graphics protocol | 未対応 | 内部サポート |
| Unicode/グラフェム | 基本的 | 高品質 (ICU ベース) |
| 256色/TrueColor | 対応 | 対応 |
| カーリー下線 | 未対応 | 対応 (5種類) |
| ハイパーリンク (OSC 8) | 未対応 | フルサポート |
| Shell integration | 未対応 | OSC 133 サポート |
| スクロールバック効率 | リングバッファ | mmap ページリスト |
| メモリ効率 | 良好 | 優秀 (64-bit packed cell) |
| 開発の活発さ | 低頻度更新 | 活発 |

---

## 7. Emacs ソースコード検証結果

`emacs-mirror/emacs` の最新 HEAD (2026-02-26 時点) に対して
`src/emacs-module.h.in` + `src/module-env-{25..31}.h` +
`test/src/emacs-module-resources/mod-test.c` を精査し、
プランで使用する全 API の存在と署名を確認した。

### 7.1 最小 Emacs バージョン要件

**最小要求バージョン: Emacs 30**

新しい Elisp 機能・マクロを積極的に活用する方針とし、
レガシーバージョンの互換コードは書かない。

| API | 導入バージョン | 本プランでの用途 | ステータス |
|-----|---------------|-----------------|-----------|
| `intern`, `funcall`, `make_function` | Emacs 25 | 関数登録・呼出 | 使用 |
| `make_user_ptr`, `get_user_ptr` | Emacs 25 | ターミナル状態保持 | 使用 |
| `make_string`, `copy_string_contents` | Emacs 25 | 文字列操作 | 使用 |
| `extract_integer`, `make_integer` | Emacs 25 | 数値受渡 | 使用 |
| `non_local_exit_signal` | Emacs 25 | エラー報告 | 使用 |
| `should_quit` | Emacs 26 | C-g 検出 | 使用 |
| `process_input` | Emacs 27 | 描画ループ中の入力処理 | 使用 |
| `extract_time`, `make_time` | Emacs 27 | タイマー精度向上 | 使用 |
| `open_channel` | Emacs 28 | 非同期パイプ I/O | 使用 |
| `make_interactive` | Emacs 28 | インタラクティブ関数登録 | 使用 |
| `make_unibyte_string` | Emacs 28 | バイナリ文字列処理 | 使用 |
| (Emacs 29-30) | — | モジュール API 追加なし | — |

### 7.2 検証で修正した問題

| # | 問題 | 修正内容 |
|---|------|---------|
| 1 | `should_quit` の戻り値型 | `!= emacs_funcall_exit_return` → `bool` 直接評価 |
| 2 | `extract_integer` の戻り値型 | `int` → `intmax_t` |
| 3 | `bind_function` の実装 | Emacs 公式テストに合わせ `defalias` を使用 |
| 4 | DEFUN マクロに `data` 引数がなかった | `data` パラメータ追加 |
| 5 | `env->size` チェックが欠落 | `emacs_module_init` に追加 |
| 6 | `provide` ヘルパー関数がなかった | `bind_function` と共にヘルパーとして追加 |

### 7.3 Emacs 28-30 の Elisp 新機能の活用

Emacs 30 を最小バージョンとすることで、以下の新機能を積極的に使用する。

**Emacs 28:**
- native compilation (`native-comp`) — モジュールのロードと Elisp 側の高速化
- `make_unibyte_string` (module API) — バイナリデータの直接渡し
- `open_channel` (module API) — バックグラウンドスレッドからの非同期書き込み

**Emacs 29:**
- `keymap-set` / `keymap-unset` — `define-key` を置き換え
- `defvar-keymap` — キーマップ定義の簡潔化
- `use-package` 組み込み — パッケージ設定の標準化
- `setopt` — `defcustom` 変数の型安全な設定
- `pos-bol` / `pos-eol` — `line-beginning-position` / `line-end-position` の置き換え
- `with-restriction` — ナローイングの安全なスコープ管理
- `string-search` — 文字列検索の簡潔化

**Emacs 30:**
- `cl-generic` の改善、`pcase` パターンの拡張
- `use-package` の `:vc` キーワード — Git からの直接インストール
- `completion-metadata` の改善 — 補完システム連携
- `buffer-match-p` の拡張 — バッファマッチング条件の柔軟化
