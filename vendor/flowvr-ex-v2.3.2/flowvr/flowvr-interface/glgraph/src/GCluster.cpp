/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                    Graph Visualization Tool                     *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA                                                           *
 *  ALL RIGHTS RESERVED.                                           *
 *                                                                 *
 * This source is covered by the GNU GPL, please refer to the      *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Jeremie Allard,                                              *
 *    Thomas Arcila,                                               *
 *    Antoine Meler                                                *
 *    Clement Meniers,                                             *
 *    Bruno Raffin,                                                *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: ./src/utils.cpp                                           *
 *                                                                 *
 * Contacts: Xavier Martin <xavier.martin@imag.fr>                 *
 *                                                                 *
 ******************************************************************/

#include "GCluster.h"
#include "Graph.h"


#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif


#include <list>
#include "interface.h"

#include "utils.h"

using namespace std;

bool leftPredicate(float x, float xBound) {
	return x < xBound;
}
bool rightPredicate(float x, float xBound) {
	return x > xBound;
}
bool topPredicate(float y, float yBound) {
	return y < yBound;
}
bool bottomPredicate(float y, float yBound) {
	return y > yBound;
}

bool GCluster::isClusterVisible(Viewer* v) {
	/*
	qglviewer::Vec center = v->camera()->sceneCenter();
	int v_x = center[0];
	int v_y = center[1];
	int height = v->camera()->screenHeight();
	int width = v->camera()->screenWidth();


	float top = v_y - height/2.0;
	float bottom = v_y + height/2.0;
	float left = v_x - width/2.0;
	float right = v_x + width/2.0;

	//v->displayInfo(toStr(y1));

	bool visible = false;

	bool predicate = false;
	// Upper left corner
	predicate = leftPredicate(x1, left) || rightPredicate(x1, right)
			|| topPredicate(y1, top) || bottomPredicate(y1, bottom);
	if (predicate == false) {
		visible = true;
	}

	predicate = leftPredicate(x2, left) || rightPredicate(x2, right)
			|| topPredicate(y2, top) || bottomPredicate(y2, bottom);
	if (predicate == false) {
		visible = true;
	}

	predicate = leftPredicate(x1, left) || rightPredicate(x1, right)
			|| topPredicate(y2, top) || bottomPredicate(y2, bottom);
	if (predicate == false) {
		visible = true;
	}

	predicate = leftPredicate(x2, left) || rightPredicate(x2, right)
			|| topPredicate(y1, top) || bottomPredicate(y1, bottom);
	if (predicate == false) {
		visible = true;
	}

	// Special case: box bigger

	 */


	return true;
}

GNode* GCluster::findNodeByName(string name, Viewer* v, string fullName) {
	//v->displayInfo("findNodeByName: "+name);
	int pos = name.find_first_of("/");
	if (pos == string::npos) {
		// {node should be local to the cluster}
		Component* comp;
		for (list<GNode*>::iterator n=localNodes.begin(); n!=localNodes.end(); n++ ) {
			/*
			 WORKS ONLY FOR PRIMITIVE COMPONENTS
			 */
			comp = (*n)->component;
			if (comp != NULL) {
				// {Node is mapped to a component}
				if (comp->label != NULL) {
					// {node is a primitive}
					string lastName = "";
					lastName.append(comp->label);
					int stripPos = lastName.find_last_of("/");
					lastName = lastName.substr(stripPos+1);
					if (lastName == name) {
						// {we've found our node}
						return (*n);
					}
				} else {
					// {node is a filter/connection}
					if (comp->id == fullName) {
						// {we've found our node}
						return (*n);
					}
				}
			}
		}
		// {not found, does not exist}
		return NULL;
	} else {
		//v->displayInfo("primitive is in a subcluster");
		// {primitive is in a subcluster}
		string clusterName = name.substr(0, pos);
		for (list<GCluster*>::iterator c=subClusters.begin(); c!=subClusters.end(); c++) {
			//v->displayInfo("iterating over "+(*c)->label);
			int pos2 = (*c)->label.find_last_of("/");
			string currentClusterName = (*c)->label.substr(pos2+1);
			//v->displayInfo("currentClusterName: "+currentClusterName);
			//v->displayInfo("clusterName: "+clusterName);
			if (currentClusterName == clusterName) {
				// {we've found our cluster}
				return (*c)->findNodeByName(name.substr(pos+1), v, fullName);
			}
		}
		// {cluster doesn't exist}
		return NULL;
	}
}

GCluster* GCluster::findClusterByName(std::string name, Viewer* v) {
	//v->displayInfo("findClusterByName: "+name);
	//v->displayInfo("label: "+this->fullLabel);
	string currentClusterName = this->fullLabel;

	if (currentClusterName == name) {
		// {current cluster is target cluster}
		//v->displayInfo("cluster found: "+currentClusterName);
		return this;
	} else {
		// {target cluster is in one of the children}
		GCluster* targetCl = NULL;
		for (list<GCluster*>::iterator itCl=subClusters.begin(); itCl!=subClusters.end() && targetCl == NULL; itCl++) {
			targetCl = (*itCl)->findClusterByName(name, v);
		}
		return targetCl;
	}
}

bool pointInsideCluster(float x, float y, GCluster* cluster, Viewer* v) {
	//v->displayInfo("pointInside: "+cluster->label);

	//v->displayInfo(toStr(x)+" "+toStr(y));
	//v->displayInfo(toStr(cluster->x1)+" "+toStr(cluster->y1)+" "+toStr(cluster->x2)+" "+toStr(cluster->y2) );

	if (x >= cluster->x1 && x <= cluster->x2 && y <= cluster->y2 && y >= cluster->y1)
		return true;
	else
		return false;
}

GNode* GCluster::findNodeByPosition(float x, float y, Viewer* v) {
	// Browse cluster

	// Is it in one of the child clusters?
	for (list<GCluster*>::iterator childCluster=subClusters.begin(); childCluster!=subClusters.end(); childCluster++) {
		// Perform boundary check
		if ( pointInsideCluster(x,y,(*childCluster), v) ) {
			v->displayInfo("Inside "+(*childCluster)->label);
			return (*childCluster)->findNodeByPosition(x,y,v);
		}
	}
	// {it isn't in a child cluster}

	// Is it a local node?
	for (list<GNode*>::iterator localNode=localNodes.begin(); localNode!=localNodes.end(); localNode++) {
		// It is -> return node;
		return (*localNode);
	}
	// If we reach this point, we clicked a white spot -> return NULL;
	return NULL;
}

void GCluster::fillHostsVector(set<string>& hosts) {
	// Check local cluster nodes' host
	for (list<GNode*>::iterator node=localNodes.begin(); node!=localNodes.end(); node++) {
		// Is the node a primitive?
		if ((*node)->component != NULL) {
			// {node is a primitive}
			// Browse through it's hosts
			Component* comp = (*node)->component;
			for (list<Host*>::iterator h=comp->hosts.begin(); h!=comp->hosts.end(); h++) {
				// We have a host, insert it into the set (which will handle redundancy)
				hosts.insert(string((*h)->value));
			}
		}
	}
	// {handled localNodes hosts}
	// Browse through subclusters
	for (list<GCluster*>::iterator itCl=subClusters.begin(); itCl!=subClusters.end(); itCl++) {
		(*itCl)->fillHostsVector(hosts);
	}
	// {handled subClusters hosts}
}

void GCluster::fillHostPairList(vector< pair<string,Q3ListViewItem*> >& hosts) {
	// Check local cluster nodes
	for (list<GNode*>::iterator node=localNodes.begin(); node!=localNodes.end(); node++) {
		// Is the node a primitive?
		if ((*node)->component != NULL) {
			// {node is a primitive}
			// Browse through it's hosts
			Component* comp = (*node)->component;
			for (list<Host*>::iterator h=comp->hosts.begin(); h!=comp->hosts.end(); h++) {
				// We have a host, find the right Q3ListViewItem* from it's name
				string hostName = string((*h)->value);
				// Browse through our host entries, stop at the end or when we've found it
				for (vector< pair<string,Q3ListViewItem*> >::iterator pair=hosts.begin(); pair!=hosts.end(); pair++) {
					// Does the pair match?
					if ((*pair).first == hostName) {
						// {pair has the list we're looking for}
						Q3ListViewItem* item = new Q3ListViewItem((*pair).second, (*node)->componentLabel.c_str());
						// {we've added the node to the corresponding host}
					}
				}
				// {done browsing through the pair list}
			}
			// {done considering all the primitive's hosts}
		}
	}
	// {done localNodes hosts}

	// Browse through subclusters
	for (list<GCluster*>::iterator itCl=subClusters.begin(); itCl!=subClusters.end(); itCl++) {
		(*itCl)->fillHostPairList(hosts);
	}
	// {handled subClusters nodes}
}



void GCluster::draw(Viewer* v, bool drawChildren, bool drawNodes, bool drawEdges, bool drawPorts) {
	// Inside
	//v->displayInfo("Cluster draw");

	//v->displayInfo(toStr(x1)+" "+toStr(y1) + " "+toStr(x2) + " "+toStr(y2));

	if (!(x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0)) {

		glColor3f(fillColor[0], fillColor[1], fillColor[2]);
		glBegin( GL_QUADS);
			glVertex3f(x1, y1, depth);
			glVertex3f(x2, y1, depth);
			glVertex3f(x2, y2, depth);
			glVertex3f(x1, y2, depth);
		glEnd();

		// Outline
		glColor3f(outlineColor[0], outlineColor[1], outlineColor[2]);
		glBegin( GL_LINE_LOOP);
			glVertex3f(x1, y1, depth+1);
			glVertex3f(x2, y1, depth+1);
			glVertex3f(x2, y2, depth+1);
			glVertex3f(x1, y2, depth+1);
		glEnd();
	}

	glColor3f(0.f, 0.f, 0.f);
	renderStrokeLineFit((x1+x2)/2, y2-17.0, depth+1, toChar(label), (x2-x1), 0);


	// Ports
	if (drawPorts) {
		if (inputPorts != NULL)
			inputPorts->draw(v);
		if (outputPorts != NULL)
			outputPorts->draw(v);
	}
	// Nodes
	if (drawNodes) {
		for (list<GNode*>::iterator n = localNodes.begin(); n != localNodes.end(); n++) {
			(*n)->draw();
		}
	}
	// Edges
	if (drawEdges) {
		for (list<GEdge*>::iterator e = localEdges.begin(); e != localEdges.end(); e++) {
			(*e)->draw();
		}
	}
	// Children
	if (drawChildren) {
		for (list<GCluster*>::iterator c = subClusters.begin(); c
				!= subClusters.end(); c++) {
			(*c)->draw(v);
		}
	}

}

void GCluster::drawForMiniViewer(Viewer* v) {
	if (minivGenerationPhasePassed) {
		if (miniViewerDL != 0) {
			glCallList(miniViewerDL);
			for (list<GCluster*>::iterator c = subClusters.begin(); c
										!= subClusters.end(); c++) {
				(*c)->drawForMiniViewer(v);
			}
		} else {
			glColor3f(fillColor[0], fillColor[1], fillColor[2]);
			glBegin( GL_QUADS);
				glVertex3f(x1, y1, depth);
				glVertex3f(x2, y1, depth);
				glVertex3f(x2, y2, depth);
				glVertex3f(x1, y2, depth);
			glEnd();

			// Outline
			glColor3f(outlineColor[0], outlineColor[1], outlineColor[2]);
			glLineWidth(3.0f);
			glBegin( GL_LINE_LOOP);
				glVertex3f(x1, y1, depth);
				glVertex3f(x2, y1, depth);
				glVertex3f(x2, y2, depth);
				glVertex3f(x1, y2, depth);
			glEnd();

			for (list<GCluster*>::iterator c = subClusters.begin(); c
							!= subClusters.end(); c++) {
						(*c)->drawForMiniViewer(v);
			}
		}
	} else {
		miniViewerDL = glGenLists(1);
		if (miniViewerDL != 0) {
			glNewList(miniViewerDL, GL_COMPILE);
				glColor3f(fillColor[0], fillColor[1], fillColor[2]);
				glBegin( GL_QUADS);
					glVertex3f(x1, y1, depth);
					glVertex3f(x2, y1, depth);
					glVertex3f(x2, y2, depth);
					glVertex3f(x1, y2, depth);
				glEnd();

				// Outline
				glColor3f(outlineColor[0], outlineColor[1], outlineColor[2]);
				glLineWidth(3.0f);
				glBegin( GL_LINE_LOOP);
					glVertex3f(x1, y1, depth);
					glVertex3f(x2, y1, depth);
					glVertex3f(x2, y2, depth);
					glVertex3f(x1, y2, depth);
				glEnd();

			glEndList();
		}
		minivGenerationPhasePassed = true;
		drawForMiniViewer(v);
	}
}

void GCluster::setOutlineColor(int r, int g, int b) {
	outlineColor[0]=float(r)/255.0;
	outlineColor[1]=float(g)/255.0;
	outlineColor[2]=float(b)/255.0;
}



void GCluster::setFillColor(int r, int g, int b) {
	fillColor[0]=float(r)/255.0;
	fillColor[1]=float(g)/255.0;
	fillColor[2]=float(b)/255.0;
}

std::string GCluster::getId() const
{
    return id;
}

void GCluster::setId(std::string id)
{
    this->id = id;
}


