

-- Including class.lua since the require and set path calls for lua tend to fail for some people, who knows why.
-- class.lua (http://lua-users.org/wiki/SimpleLuaClasses)
function class(base,ctor)
  local c = {}     -- a new class instance
  if not ctor and type(base) == 'function' then
      ctor = base
      base = nil
  elseif type(base) == 'table' then
   -- our new class is a shallow copy of the base class!
      for i,v in pairs(base) do
          c[i] = v
      end
      c._base = base
  end
  -- the class will be the metatable for all its objects,
  -- and they will look up their methods in it.
  c.__index = c

  -- expose a ctor which can be called by <classname>(<args>)
  local mt = {}
  mt.__call = function(class_tbl,...)
    local obj = {}
    setmetatable(obj,c)
    if ctor then
       ctor(obj,unpack(arg))
    else 
    -- make sure that any stuff from the base class is initialized!
       if base and base.init then
         base.init(obj,unpack(arg))
       end
    end
    return obj
  end
  c.init = ctor
  c.is_a = function(self,klass)
      local m = getmetatable(self)
      while m do 
         if m == klass then return true end
         m = m._base
      end
      return false
    end
  setmetatable(c,mt)
  return c
end


-- - - - - - - - - - - - - - - - - - - - -


-- ================================
-- 	netClient class
-- ================================

-- Author Yoda

-- This class simplifies management of multiple simultaneous incoming connections
-- Has basic password protection but does not implement any compression or encryption

netClient = class(function(nc, con) nc:constructor(con) end)

function netClient:constructor( con )

	self.password_required = password_required; 	-- boolean if we require a password
	self.connection = con;					-- our connection object
	self.t_received = g_time;				-- the last time we received a msg, for timeout purposes
	
	-- Configure for no blocking operations
	if self.connection then
		self.connection:settimeout(0.0);
		self.connection:setoption("tcp-nodelay",true);
	end
	
end

function netClient:transmit( data )

	if not self.password_required then

		if self.connection then
			self.connection:send(data);
		end
		
	end
	
end

function netClient:receive()

	local received_data = {};
	
	-- Check for password sent
	if self.password_required and self.connection then
		local Line = self.connection:receive('*l');
		if Line==listener_password then
			self.password_required = false;
		end
	end
	
	if not self.password_required then
		if self.connection then
			local Line = self.connection:receive('*l');
			while nil~=Line do
				self.t_received=g_time;
				if Line~="" then
					received_data[1+#received_data] = Line;
				end
				Line=self.connection:receive('*l');
			end
		end				
	end
	
	return received_data;
	
end

function netClient:isConnected()
	if g_time-self.t_received>1.0 then 
		return false;
	end
	return true;	
end

-- Can optionally be called before destroying the object
function netClient:destructor()
	if self.connection then 
		self.connection:close(); 
	end
end