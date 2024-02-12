---
editor_options: 
  markdown: 
    wrap: 72
---

# aifeducation 0.3.1

**Graphical User Interface Aifeducation Studio**

-   Added a shiny app to the package that serves as a graphical user
    interface.

**Transformer Models**

-   Fixed a bug in all transformers except BERT concerning the
    unk_token.

**TextEmbeddingModel**

-   Added a method for 'fill-mask'.
-   Added a new argument to the method 'encode', allowing to chose
    between encoding into token ids or into token strings.
-   Added a new argument to the method 'decode', allowing to chose
    between decoding into single tokens or into plain text.
-   Fixed a bug for embedding texts when using pytorch. The fix should
    decrease computational time and enables gpu support (if available on machine).
-   Fixed two missing columns for saving the results of sustainability tracking on machines
    without gpu.
-   Implemented the advantages of datasets from the python library 'datasets' increasing
    computational speed and allowing the use of large datasets.

**TextEmbeddingClassifiers**

-   Adding support for pytorch without the need for kerasV3 or
    keras-core. Classifiers for pytorch are now implemented in native
    pytorch.
-   Changed the architecture for new classifiers and extended the
    abilities of neural nets by adding the possibility to add positional
    embedding.
-   Changed the architecture for new classifiers and extended the
    abilities of neural nets by adding an alternative method for the
    self-attention mechanism via fourier transformation (similar to
    FNet).
-   Added balanced_accuracy as the new metric for determining which
    state of a model predicts classes best.
-   Fixed error that training history is not saved correctly.
-   Added a record metric for the test dataset to training history with
    pytorch.
-   Added the option to balance class weights for calculating training
    loss according to the Inverse Frequency method. Balance class
    weights is activated by default.
-   Added a method for checking the compatibility of the underlying
    TextEmbeddingModels of a classifier and an object of class
    EmbeddedText.
-   Added precision, recall, and f1-score as new metrics.

**Python Installation** - Added an argument to 'install_py_modules',
allowing to choose which machine learning framework should be
installed. - Updated 'check_aif_py_modules'.

**Further Changes**

-   Setting the machine learning framework at the start of a session is
    no longer necessary. The function for setting the global
    ml_framework remains active for convenience. The ml_framework can
    now be switched at any time during a session.
-   Updated documentation.

# aifeducation 0.3.0

-   Added DeBERTa and Funnel-Transformer support.
-   Fixed issues for installing the required python packages.
-   Fixed issues in training transformer models.
-   Fixed an issue for calculating the final iota values in classifiers
    if pseudo labeling is active.
-   Added support for PyTorch and Tensorflow for all transformer models.
-   Added support for PyTorch for classifier objects via keras 3 in the
    future.
-   Removed augmentation of vocabulary from training BERT models.
-   Updated documentation.
-   Changed the reported values for kappa.

# aifeducation 0.2.0

-   First release on CRAN
