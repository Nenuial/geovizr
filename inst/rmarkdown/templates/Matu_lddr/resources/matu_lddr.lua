function sectitle (el)
  local classes = el.classes or el.attr.classes

  if not classes:includes("section") then
    return nil
  else
    return pandoc.RawInline('tex', "\\sectitle{" .. pandoc.utils.stringify(el.content) .. "}")
  end
end

function exadiv (el)
  local classes = el.classes or el.attr.classes

  if not classes:includes("questiontable") then
    return nil
  else
    return pandoc.walk_block(el, {
      Para = function (el)
        local para = el.content

        for i=1,#para do
          if para[i].text:match '%d+|' then
            local point = string.match(para[i].text, '(%d+)|')
            para[i] = pandoc.RawInline('tex', '\\exaquestion{' .. point .. '}{')
            table.insert(para, pandoc.RawInline('tex', '}'))
            para:remove(i+1)
            el.content = para
            return el
          end
        end
      end
    })
  end
end

return {{Header = sectitle}, {Div = exadiv}}
