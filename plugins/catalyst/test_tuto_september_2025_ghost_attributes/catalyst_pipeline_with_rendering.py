# script-version: 2.0
from paraview.simple import *
from paraview import catalyst
import time

# registrationName must match the channel name used in the
# 'CatalystAdaptor'.
producer = TrivialProducer(registrationName="grid")

# ----------------------------------------------------------------
# setup views used in the visualization
# ----------------------------------------------------------------

jj_ny = 60 + 2
jj_nx = 12 + 2

div_jj_nx = jj_nx/2
div_jj_ny = jj_ny/2

jj_pos = 1.2*jj_nx*900/120
jj_focal_point = 2*jj_nx*240/120

# ######## render view temperature

# Create a new 'Render View'
renderView1 = CreateView('RenderView')
renderView1.ViewSize = [1000,1600]
renderView1.CameraPosition = [div_jj_nx, div_jj_ny, jj_pos]
renderView1.CameraFocalPoint = [div_jj_nx,  div_jj_ny, jj_focal_point]
renderView1.CameraParallelScale = 100

# get color transfer function/color map for 'temperature'
temperatureLUT = GetColorTransferFunction('temperature')
## RGB: first line: min value, last line: max value
temperatureLUT.RGBPoints = [0.0, 0.231373, 0.298039, 0.752941, 
                        100.0, 0.865003, 0.865003, 0.865003,
                        200.0, 0.705882, 0.0156863, 0.14902]
temperatureLUT.ScalarRangeInitialized = 1.0

# show data from grid
# gridDisplay222 = Show(producer, renderView1, 'UnstructuredGridRepresentation')
gridDisplay222 = Show(producer, renderView1, 'StructuredGridRepresentation')

gridDisplay222.Representation = 'Surface With Edges'
gridDisplay222.ColorArrayName = ['CELLS', 'temperature']
gridDisplay222.LookupTable = temperatureLUT

# get color legend/bar for temperatureLUT in view renderView1
temperatureLUTColorBar = GetScalarBar(temperatureLUT, renderView1)
temperatureLUTColorBar.Title = 'temperature'
temperatureLUTColorBar.ComponentTitle = 'Magnitude'

# set color bar visibility
temperatureLUTColorBar.Visibility = 1

# show color legend
gridDisplay222.SetScalarBarVisibility(renderView1, True)


# # ----------------------------------------------------------------
# # setup extractors
# # ----------------------------------------------------------------

SetActiveView(renderView1)
# create extractor
pNG2= CreateExtractor('PNG', renderView1, registrationName='PNG2')
# trace defaults for the extractor.
pNG2.Trigger = 'TimeStep'

# init the 'PNG' selected for 'Writer'
pNG2.Writer.FileName = 'temperature_screenshot_{timestep:06d}.png'
pNG2.Writer.ImageResolution = [1000,1600]
pNG2.Writer.Format = 'PNG'


# ------------------------------------------------------------------------------
# Catalyst options
options = catalyst.Options()
## 0: pas de client, generration des images
## 1: interactif
options.EnableCatalystLive = 0


# Greeting to ensure that ctest knows this script is being imported
print("#############################################################")
print("executing catalyst_pipeline")
print("#############################################################")
def catalyst_execute(info):
    global producer
    producer.UpdatePipeline()
    print("-----------------------------------")
    print("executing (cycle={}, time={})".format(info.cycle, info.time))
    print("bounds:", producer.GetDataInformation().GetBounds())
    print("temperature-range:", producer.CellData["temperature"].GetRange(0))
    # In a real simulation sleep is not needed. We use it here to slow down the
    # "simulation" and make sure ParaView client can catch up with the produced
    # results instead of having all of them flashing at once.
    if options.EnableCatalystLive:
        time.sleep(1)
