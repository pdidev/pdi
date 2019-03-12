.. flowvr-appy documentation master file, created by
   sphinx-quickstart on Wed Apr  3 17:03:44 2013.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.
   

Welcome to flowvr-appy's documentation
======================================

----------
Navigation
----------




.. toctree::
   :maxdepth: 2

   flowvrapp
   filters
   traces
::

   import flowvrapp
   import filters
   import traces

The main point of entry is the **flowvrapp** package (flowvrapp.py). It contains what you need to describe FlowVR modules and networks.

The **filters** package contains standard filters such as Merge and PreSignal.

The **traces** package allows you to output an execution trace. Using the *gltrace* utility, you may replay the execution later and observe what messages were exchanged.



--------
Tutorial
--------
If you haven't already, `install and test FlowVR`_.

.. _install and test FlowVR: https://wiki-flowvr.imag.fr/wiki/GettingStarted

When you're all set, follow this step-by-step `FlowVR-Appy Tutorial`_. It will show you how to create FlowVR applications.

.. HINT::
   The Quick Search box on the left is the perfect entry point if you're looking for something specific, such as a "GreedyMultiple" filter.

.. _FlowVR-Appy Tutorial: https://wiki-grimage.imag.fr/flowvr-doc/py-flowvrapp-doc/flowvrapp_doc.html

------------------
Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

