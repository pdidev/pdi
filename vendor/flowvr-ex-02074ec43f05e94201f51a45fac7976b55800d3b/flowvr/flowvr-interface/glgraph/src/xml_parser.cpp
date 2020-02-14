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
 * File: ./src/xml_parser.cpp                                      *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#include <iostream>
#include "xml_parser.h"

bool is_empty(xmlChar* str) {
	for (int i = 0; i < xmlStrlen(str); i++) {
		if (str[i] != xmlChar(' ') && str[i] != xmlChar('\n'))
			return false;
	}
	return true;
}

void processNode(xmlTextReaderPtr reader, Q3ListViewItem** item) {
	xmlChar *name, *value;
	bool parent = false;
	name = xmlTextReaderName(reader);
	if (name == NULL)
	name = xmlStrdup(BAD_CAST "--");
	value = xmlTextReaderValue(reader);

	//   printf("%d %d %s %d",
	//          xmlTextReaderDepth(reader),
	//          xmlTextReaderNodeType(reader),
	//          name,
	//          xmlTextReaderIsEmptyElement(reader));


	if (xmlTextReaderNodeType(reader) == 1) {
		//*item = new QListViewItem(*item, QString((const char*)xmlTextReaderName(reader)) );

		if (xmlTextReaderIsEmptyElement(reader))
			parent = true;

		QString id = "";
		int no_attribute = 0;
		QString label = (const char*) name;
		while (xmlTextReaderMoveToNextAttribute(reader)) {
			QString attrib_name = QString(
					(const char*) xmlTextReaderName(reader));
			QString attrib_value = QString(
					(const char*) xmlTextReaderValue(reader));

			if (no_attribute == 0)
				label += QString(" [");
			else
				label += QString(", ");
			label += attrib_name + QString("=") + attrib_value;

			if (attrib_name == QString("id")) {
				id = attrib_value;
			}

			no_attribute++;
		}
		if (no_attribute > 0)
			label += QString("]");

		*item = new Q3ListViewItem(*item, label);
		if (id.length() > 0) {
			(*item)->setText(2, id);
			if (QString((const char*) name) == "connection" || QString(
					(const char*) name) == "connectionstamps" || QString(
					(const char*) name) == "module" || QString(
					(const char*) name) == "synchronizer" || QString(
					(const char*) name) == "filter" || QString(
					(const char*) name) == "routingnode" || QString(
					(const char*) name) == "routingnodestamps")

				(*item)->setText(1, id);
		}
	}
	xmlFree(name);

	if (value != NULL) {
		QString str((const char*) value);
		if (!is_empty(value))
			new Q3ListViewItem(
					*item,
					QString("\"") + QString((const char*) value)
							+ QString("\""));
		xmlFree(value);
	}

	if (xmlTextReaderNodeType(reader) == 15 || parent) {
		*item = (*item)->parent();
	}

}

int streamXmlFile(const char *filename, Q3ListViewItem* item) {
	xmlTextReaderPtr reader;
	int ret;

	reader = xmlNewTextReaderFilename(filename);
	if (reader != NULL) {
		ret = xmlTextReaderRead(reader);
		while (ret == 1) {
			processNode(reader, &item);
			ret = xmlTextReaderRead(reader);
		}
		xmlFreeTextReader(reader);
		if (ret != 0) {
			printf("%s : failed to parse\n", filename);
			return 0;
		}
	} else {
		printf("Unable to open %s\n", filename);
		return 0;
	}

	return 1;
}

