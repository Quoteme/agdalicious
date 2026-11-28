-- Automatic \-abbreviation expansion in insert mode, in the spirit of
-- Agda's emacs input method:
--   * an abbreviation that cannot be extended any further is replaced as soon
--     as it is complete (\lambda -> λ),
--   * an ambiguous one (\to, which could become \top) is replaced once the next
--     typed character rules out the longer ones (\to<space> -> → ).
local M = {}

local function build(config)
  local abbrevs = vim.tbl_extend("force", require("agdalicious.abbreviations"), config.extra or {})
  -- every proper prefix of every key; "is something longer still possible?"
  local prefixes = {}
  for key in pairs(abbrevs) do
    for i = 1, #key - 1 do
      prefixes[key:sub(1, i)] = true
    end
  end
  return abbrevs, prefixes
end

--- Replace leader+abbrev at bytes [start, start+len) on the cursor line with `sym`.
local function replace(buf, row, start, len, sym, cursor_col)
  vim.api.nvim_buf_set_text(buf, row, start, row, start + len, { sym })
  vim.api.nvim_win_set_cursor(0, { row + 1, cursor_col - len + #sym })
end

function M.attach(buf, config)
  local leader = config.leader
  local abbrevs, prefixes = build(config)

  --- The pending abbreviation before the cursor: byte offset of the leader and the typed text.
  local function pending()
    local row, col = unpack(vim.api.nvim_win_get_cursor(0))
    local before = vim.api.nvim_get_current_line():sub(1, col)
    local start = before:match(".*()" .. vim.pesc(leader))
    if not start then
      return nil
    end
    return row - 1, col, start - 1, before:sub(start + #leader)
  end

  local function on_change()
    local row, col, start, typed = pending()
    if not typed or typed == "" then
      return
    end
    if abbrevs[typed] and not prefixes[typed] then
      -- complete and unambiguous: expand right away
      replace(buf, row, start, #leader + #typed, abbrevs[typed], col)
      return
    end
    if not abbrevs[typed] and not prefixes[typed] then
      -- the last character ended the abbreviation; expand what came before it
      local last = vim.fn.strcharpart(typed, vim.fn.strchars(typed) - 1)
      local abbr = typed:sub(1, #typed - #last)
      if abbrevs[abbr] then
        replace(buf, row, start, #leader + #abbr, abbrevs[abbr], col)
      end
    end
  end

  local function on_leave()
    local row, col, start, typed = pending()
    if typed and abbrevs[typed] then
      replace(buf, row, start, #leader + #typed, abbrevs[typed], col)
    end
  end

  local group = vim.api.nvim_create_augroup("agdalicious.unicode." .. buf, { clear = true })
  vim.api.nvim_create_autocmd("TextChangedI", { group = group, buffer = buf, callback = on_change })
  vim.api.nvim_create_autocmd("InsertLeave", { group = group, buffer = buf, callback = on_leave })
end

return M
