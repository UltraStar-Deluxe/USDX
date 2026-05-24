USDX supports plugins written in lua. These must be placed with a `.usdx` extension in the `plugins` directory.

They're mostly used in Party Mode, but they _can_ be used outside of Party Mode like so:
```
-- minimum time between runs in milliseconds
local Delay = 1000

local LastDraw = 0

function plugin_init()
  register('Dummy plugin', '1.00', 'PluginAuthor')
  hDraw = Usdx.Hook('Display.Draw', 'MyEveryFrameFunction')
  return true
end

function MyEveryFrameFunction()
  -- whatever you put in here runs every frame, hence the limiter
  local Now = Usdx.Time()
  if LastDraw + Delay > Now then
    return
  end

  -- whatever you put between here and the end of the function runs _close enough_ to once a second

  LastDraw = Now
end
```

Available globals inside plugins:
- `current_filename`: absolute path to the plugin file