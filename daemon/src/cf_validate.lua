function is_non_empty_string (s)
   return (s ~= '')
end

function validate (n, rt, chk)
   v = _G[n]
   if v then
      t = type(v)
      if t ~= 'function' and t ~= rt then
	 error(string.format('value %q not of required type %q', n, rt))
      end
      if chk then
	 if not chk(v) then
	    error(string.format('value %q is not valid', n))
	 end
      end
   end
end

validate('username', 'string', cl2.is_ascii_ident)
validate('upload_url', 'string', is_non_empty_string)
validate('remokon_host', 'string', is_non_empty_string)
validate('remokon_port', 'number', nil)
validate('remokon_password', 'string', is_non_empty_string)
validate('jid', 'string', is_non_empty_string)
validate('iap', 'number', nil)
validate('database_dir_string', 'string', is_non_empty_string)
validate('database_disk_threshold', 'number', nil)
