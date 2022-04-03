function tabto (el)
  local classes = el.classes or el.attr.classes

  if not classes:includes("tabto") then
    return nil
  else
    return pandoc.RawInline('tex', "\\tabto{" .. el.content[1].text .. "}")
  end
end

return {{Span = tabto}}
