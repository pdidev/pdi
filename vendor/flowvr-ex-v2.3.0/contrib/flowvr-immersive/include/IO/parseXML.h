

struct screen_info{
	int id;
	int width;
	int height;
	float bottomleft[3];
	float bottomright[3];
	float topleft[3];
	int * window_pos;

	screen_info()
	{
		id = 0;
		width = 0;
		height = 0;
		bottomleft[0] = 0.0f;
		bottomleft[1] = 0.0f;
		bottomleft[2] = 0.0f;
		bottomright[0] = 0.0f;
		bottomright[1] = 0.0f;
		bottomright[2] = 0.0f;
		topleft[0] = 0.0f;
		topleft[1] = 0.0f;
		topleft[2] = 0.0f;
	}
};

struct tracker_info{
	float pre_mat[16];
	float post_mat_left[16];
	float post_mat_right[16];
};


class ScreenParser{

public:
	ScreenParser() {}
	void parseFile(std::string filename, int id_screen);
	int getWidth();
	int getHeight();
	screen_info * getScreenInfo() const;

protected:
	screen_info * _screen;
};

class TrackerParser{

public:
	TrackerParser() {}
	void parseFile(std::string filename);
	float * getPreMat();
	float * getPostMatLeft();
	float * getPostMatRight();
	tracker_info * getTrackerInfo() const;

protected:
	tracker_info * _tracker;
};
