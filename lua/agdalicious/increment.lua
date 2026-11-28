-- <C-a> / <C-x> that also count in sub- and superscript digits:
-- x₁ → x₂, n⁹ → n¹⁰, x₀ → x₋₁ (with <C-x>). Plain numbers are left to the
-- built-in commands. Counts work, and `.` repeats.
local M = {}

local scripts = {
  { digits = { "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉" }, minus = "₋" },
  { digits = { "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹" }, minus = "⁻" },
}
local digit = {} -- character -> { script, value }
for _, s in ipairs(scripts) do
  for v, d in ipairs(s.digits) do
    digit[d] = { s, v - 1 }
  end
end

--- The number Vim would pick on `line` for a cursor at byte `col` (0-based):
--- the first one that ends after it. `script` is "ascii" for plain digits;
--- `from` and `to` are 0-based bytes, end exclusive.
local function number_at(line, col)
  local run
  for from, c in line:gmatch("()([%z\1-\127\194-\244][\128-\191]*)") do
    from = from - 1
    local d = digit[c]
    local script = d and d[1] or (c:match("^%d$") and "ascii") or nil
    if run and script == run.script then
      run.to = from + #c
      run.value = run.value * 10 + (d and d[2] or 0)
    else
      if run and run.to > col then
        return run
      end
      run = script and { script = script, from = from, to = from + #c, value = d and d[2] or 0 } or nil
    end
  end
  if run and run.to > col then
    return run
  end
end

local delta = 0

-- operatorfunc, so that `.` repeats
function M.operator()
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local line = vim.api.nvim_get_current_line()
  local n = number_at(line, col)
  if not n then
    return
  elseif n.script == "ascii" then
    return vim.cmd("normal! " .. math.abs(delta) .. (delta > 0 and "\1" or "\24"))
  end
  local s, from, value = n.script, n.from, n.value
  if from >= #s.minus and line:sub(from - #s.minus + 1, from) == s.minus then
    from, value = from - #s.minus, -value
  end
  value = value + delta
  local new = (value < 0 and s.minus or "") .. tostring(math.abs(value)):gsub("%d", function(d)
    return s.digits[d + 1]
  end)
  vim.api.nvim_buf_set_text(0, row - 1, from, row - 1, n.to, { new })
  -- like the built-in commands: end on the number's last character
  vim.api.nvim_win_set_cursor(0, { row, from + #new - #s.digits[math.abs(value) % 10 + 1] })
end

function M.attach(buf)
  for key, sign in pairs({ ["<C-a>"] = 1, ["<C-x>"] = -1 }) do
    vim.keymap.set("n", key, function()
      local n = number_at(vim.api.nvim_get_current_line(), vim.api.nvim_win_get_cursor(0)[2])
      if not n or n.script == "ascii" then
        return key
      end
      delta = sign * vim.v.count1
      vim.go.operatorfunc = "v:lua.require'agdalicious.increment'.operator"
      return "g@l"
    end, {
      buffer = buf,
      expr = true,
      desc = (sign > 0 and "Increment" or "Decrement") .. " the number at the cursor, also in sub- and superscripts",
    })
  end
end

return M
