--- Entry Point ---
local winMgr = CEGUI.WindowManager:getSingleton()
local system = CEGUI.System:getSingleton()
 
print( "--- ### script called to pop-up console window" )
local root = winMgr:loadWindowLayout("console.wnd","", "test_layouts")
system:setDefaultMouseCursor("Vanilla-Images", "MouseArrow")
system:getGUISheet():addChildWindow(root)