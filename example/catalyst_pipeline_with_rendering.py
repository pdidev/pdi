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

# ######## render view temperature

# Create a new 'Render View'
renderView1 = CreateView('RenderView')
# renderView1.Set(
#     ViewSize=[800, 600],
#     InteractionMode='2D',
#     CenterOfRotation=[20.0, 3.0, 0.0],
#     CameraPosition=[20.0, 30.0, 408.7],
#     CameraFocalPoint=[20.0, 30.0, 0.0],
#     CameraFocalDisk=1.0,
#     CameraParallelScale=32.0,
# )

renderView1.ViewSize=[800, 600]
renderView1.InteractionMode='2D'
renderView1.CenterOfRotation=[20.0, 3.0, 0.0]
renderView1.CameraPosition=[20.0, 30.0, 408.7]
renderView1.CameraFocalPoint=[20.0, 30.0, 0.0]
renderView1.CameraFocalDisk=1.0,
renderView1.CameraParallelScale=32.0


# get color transfer function/color map for 'temperature'
temperatureLUT = GetColorTransferFunction('temperature')
## RGB: first line: min value, last line: max value
# temperatureLUT.Set(
#     RGBPoints=GenerateRGBPoints(
#         range_min=0.0,
#         range_max=200.0,
#     ),
#     ScalarRangeInitialized=1.0,
# )


temperatureLUT.RGBPoints=[0.0, 0.231373, 0.298039, 0.752941,
                        500000.0, 0.865003, 0.865003, 0.865003,
                        1000000, 0.705882, 0.0156863, 0.14902]

temperatureLUT.ScalarRangeInitialized=1.0



# show data from grid
## wgridDisplay = Show(producer, renderView1, 'UnstructuredGridRepresentation')
gridDisplay = Show(producer, renderView1, 'StructuredGridRepresentation')

gridDisplay.Representation = 'Surface With Edges'
gridDisplay.ColorArrayName = ['CELLS', 'temperature']
gridDisplay.LookupTable = temperatureLUT

# get color legend/bar for temperatureLUT in view renderView1
temperatureLUTColorBar = GetScalarBar(temperatureLUT, renderView1)
temperatureLUTColorBar.Title = 'temperature'

# set color bar visibility
temperatureLUTColorBar.Visibility = 1

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
pNG2.Writer.FileName = 'temperature_screenshot_{timestep:06d}.png'
pNG2.Writer.ImageResolution=[800, 600]
pNG2.Writer.Format = 'PNG'

# # ----------------------------------------------------------------
# # setup extractor for saving the solution in VTK file
# # ----------------------------------------------------------------

extractor_vtk_file = None

mesh_grid = producer.GetClientSideObject().GetOutputDataObject(0)
if mesh_grid.IsA('vtkUnstructuredGrid'):
    extractor_vtk_file = CreateExtractor('VTU', producer, registrationName='VTU')
elif mesh_grid.IsA('vtkMultiBlockDataSet'):
    extractor_vtk_file = CreateExtractor('VTM', producer, registrationName='VTM')
elif mesh_grid.IsA('vtkPartitionedDataSet'):
    extractor_vtk_file = CreateExtractor('VTPD', producer, registrationName='VTPD')
else:
    raise RuntimeError("Unsupported data type: %s. Check that the adaptor is providing channel named %s",
                        mesh_grid.GetClassName(), "grid")


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
    print("temperature-range:", producer.CellData["temperature"].GetRange(0))
    # In a real simulation sleep is not needed. We use it here to slow down the
    # "simulation" and make sure ParaView client can catch up with the produced
    # results instead of having all of them flashing at once.
    if options.EnableCatalystLive:
        time.sleep(0.1)
