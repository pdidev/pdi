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

# Create a new 'Render View'
renderView1 = CreateView('RenderView')

renderView1.ViewSize=[800, 600]
renderView1.InteractionMode='2D'
renderView1.CenterOfRotation=[40.0, 12.0, 0.0]
renderView1.CameraPosition=[40.0, 12.0, 208.7]
renderView1.CameraFocalPoint=[5.0, 12.0, 0.0]
renderView1.CameraFocalDisk=1.0,
renderView1.CameraParallelScale=20.0

# element_rank: show the rank of the MPI proceess that owns the element of the mesh.
# get color transfer function/color map for 'element_rank'
element_rankLUT = GetColorTransferFunction('element_rank')

element_rankLUT.RGBPoints=[0.0, 0.231373, 0.298039, 0.752941,
                        2.0, 0.865003, 0.865003, 0.865003,
                        4.0, 0.705882, 0.0156863, 0.14902]

element_rankLUT.ScalarRangeInitialized=1.0


# show data from grid
gridDisplay = Show(producer, renderView1, 'StructuredGridRepresentation')

gridDisplay.Representation = 'Surface With Edges'
gridDisplay.ColorArrayName = ['CELLS', 'element_rank']
gridDisplay.LookupTable = element_rankLUT

# get color legend/bar for element_rankLUT in view renderView1
element_rankLUTColorBar = GetScalarBar(element_rankLUT, renderView1)
element_rankLUTColorBar.Title = 'element_rank'

# set color bar visibility
element_rankLUTColorBar.Visibility = 1

# show color legend
gridDisplay.SetScalarBarVisibility(renderView1, True)

# # ----------------------------------------------------------------
# # setup extractors
# # ----------------------------------------------------------------

SetActiveView(renderView1)
# create extractor
pNG2= CreateExtractor('PNG', renderView1, registrationName='PNG2')
# trace defaults for the extractor.
pNG2.Trigger = 'TimeStep'

# init the 'PNG' selected for 'Writer'
pNG2.Writer.FileName = 'element_rank_screenshot_{timestep:06d}.png'
pNG2.Writer.ImageResolution=[800, 600]
pNG2.Writer.Format = 'PNG'

# ------------------------------------------------------------------------------
# Catalyst options
options = catalyst.Options()
## 0: no client, generate the png images and vtk files.
## 1: interactive
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
    print("element_rank-range:", producer.CellData["element_rank"].GetRange(0))

    # In a real simulation sleep is not needed. We use it here to slow down the
    # "simulation" and make sure ParaView client can catch up with the produced
    # results instead of having all of them flashing at once.
    if options.EnableCatalystLive:
        time.sleep(0.1)
