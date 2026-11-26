# # script-version: 2.0
from paraview.simple import *

# Greeting to ensure that ctest knows this script is being imported
print("executing catalyst_pipeline")

# registrationName must match the channel name used in the
# 'CatalystAdaptor'.
producer = TrivialProducer(registrationName="grid")

extractor_vtk_file = None

mesh_grid = producer.GetClientSideObject().GetOutputDataObject(0)
if mesh_grid.IsA('vtkImageData'):
    extractor_vtk_file = CreateExtractor('VTI', producer, registrationName='VTI')
elif mesh_grid.IsA('vtkRectilinearmesh_grid'):
    extractor_vtk_file =  CreateExtractor('VTR', producer, registrationName='VTR')
elif mesh_grid.IsA('vtkStructuredmesh_grid'):
    extractor_vtk_file = CreateExtractor('VTS', producer, registrationName='VTS')
elif mesh_grid.IsA('vtkPolyData'):
    extractor_vtk_file = CreateExtractor('VTP', producer, registrationName='VTP')
elif mesh_grid.IsA('vtkUnstructuredmesh_grid'):
    extractor_vtk_file = CreateExtractor('VTU', producer, registrationName='VTU')
elif mesh_grid.IsA('vtkUniformmesh_gridAMR'):
    extractor_vtk_file = CreateExtractor('VTH', producer, registrationName='VTH')
elif mesh_grid.IsA('vtkMultiBlockDataSet'):
    extractor_vtk_file = CreateExtractor('VTM', producer, registrationName='VTM')
elif mesh_grid.IsA('vtkPartitionedDataSet'):
    extractor_vtk_file = CreateExtractor('VTPD', producer, registrationName='VTPD')
elif mesh_grid.IsA('vtkPartitionedDataSetCollection'):
    extractor_vtk_file = CreateExtractor('VTPC', producer, registrationName='VTPC')
elif  mesh_grid.IsA('vtkHyperTreemesh_grid'):
    extractor_vtk_file = CreateExtractor('HTG', producer, registrationName='HTG')
else:
    raise RuntimeError("Unsupported data type: %s. Check that the adaptor "
                        "is providing channel named %s",
                        mesh_grid.GetClassName(), catalystChannel)

def catalyst_execute(info):
    global producer
    producer.UpdatePipeline()
    print("-----------------------------------")
    print("executing (cycle={}, time={})".format(info.cycle, info.time))
    print("bounds:", producer.GetDataInformation().GetBounds())