#include "Application.h"
#include "interface.h"
#include <ui_myInterface.h>

void Application::loadFirstGraphAndTrace() {
	mainwidget->Viewer1->load_file_2(startGraphFile);
	
	if (startTraceFile != NULL)
		mainwidget->Viewer1->load_traces(startTraceFile);
}
