#include <QtCore>
#include <QtGui>

class Ui_myInterface;

class Application : public QApplication
{
    Q_OBJECT
public:
    Application(int &argc, char **argv)
        : QApplication(argc, argv)
    {
    	startTraceFile = NULL;
    	connect(this, SIGNAL(appStarting()), this, SLOT(loadFirstGraphAndTrace()));
        QTimer::singleShot(0, this, SIGNAL(appStarting()));
    }
    
    ~Application() {}
	
	QString startGraphFile;
	char *startTraceFile;
	Ui_myInterface *mainwidget;
 
signals:
    void appStarting();
    
public slots:
	void loadFirstGraphAndTrace();
};
