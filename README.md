
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aifeducation <a href="https://fberding.github.io/aifeducation/"><img src="https://fberding.github.io/aifeducation/logo.png" alt="aifeducation website" align="right" height="120"/></a>

<!-- badges: start -->

**GitHub** [![Project Status: Active - The project has reached a stable,
usable state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/devel%20version-1.1.6-green.svg)](https://github.com/fberding/aifeducation)
[![R-CMD-check](https://github.com/FBerding/aifeducation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FBerding/aifeducation/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/FBerding/aifeducation/graph/badge.svg)](https://app.codecov.io/gh/FBerding/aifeducation)

**CRAN** [![CRAN
status](https://www.r-pkg.org/badges/version/aifeducation)](https://CRAN.R-project.org/package=aifeducation)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/aifeducation)](https://cran.r-project.org/package=aifeducation)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-month/aifeducation?color=blue)](https://cran.r-project.org/package=aifeducation)
[![DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.aifeducation-green.svg)](https://doi.org/10.32614/CRAN.package.aifeducation)

<!-- badges: end -->

The R package *Artificial Intelligence for Education (aifeducation)* has
been designed to meet the specific needs of educators, educational
researchers, and social researchers. It is intended for those without
coding skills who wish to develop their own models, as well as for those
who wish to use models created by others. The package supports the
application of artificial intelligence (AI) to natural language
processing tasks such as text embedding and classification in
educational and social science contexts.

## Features Overview

- The simple usage of artificial intelligence provides routines for the
  most important tasks of educators and researchers in the social and
  educational sciences.
- Users can work with AI without needing to know how to code, thanks to
  the graphical user interface (AI for Education - Studio).
- It supports PyTorch as the core machine learning framework, which is
  widely used in research.
- It implements the advantages of the Python library ‘datasets’,
  increasing computational speed and enabling the use of very large
  datasets.
- Learning rates are calculated automatically.  
- It uses ‘safetensors’ to save models in ‘PyTorch’.
- Supports pre-trained language models from Hugging Face.
- It supports ModernBERT, MPNet, BERT, RoBERTa, DeBERTa, Funnel
  Transformer and more for creating context-sensitive text embeddings. A
  list of all supported models can be found
  [here](https://fberding.github.io/aifeducation/articles/a02_base_model_types.html).
- It makes sharing pre-trained models very easy.
- It integrates sustainability tracking for training and inference.
- Integrates an approach to calculating FLOPS based on the architecture
  of the model.
- Integrates special statistical techniques for handling data structures
  common in social and educational sciences.
- Supports the classification of long text documents.

Currently, the package focuses on classification tasks, which can be
used either to diagnose characteristics of learners from written
material, or to estimate the properties of learning and teaching
materials. More tasks will be implemented in the future.

## Installation

You can install the latest stable version of the package from CRAN using
the following command:

``` r
install.packages("aifeducation")
```

You can install the development version of *aifeducation* from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github(repo="FBerding/aifeducation",
                         ref="master",
                         dependencies = "Imports")
```

Further installation instructions can be found in the vignette [01 Get
Started](https://fberding.github.io/aifeducation/articles/aifeducation.html).

> Please note that an update of your version of *aifeducation* may
> require an update of your python libraries. Refer to [01 Get
> Started](https://fberding.github.io/aifeducation/articles/aifeducation.html)
> for more details.

## Graphical User Interface *AI for Education - Studio*

The package ships with a shiny app that serves as a graphical user
interface.

<figure>
<img src="https://fberding.github.io/aifeducation/home.png"
style="width:100.0%" alt="Figure 1: Aifeducation Studio" />
<figcaption aria-hidden="true">Figure 1: Aifeducation
Studio</figcaption>
</figure>

*AI for Education – Studio* allows users to develop, train, apply,
document and analyse AI models with ease, even without coding skills.
See the corresponding vignette for more details: [02 Using the graphical
user interface Aifeducation -
Studio](https://fberding.github.io/aifeducation/articles/gui_aife_studio.html).

## Sustainability

Training AI models is time-consuming and energy-intensive. To help
researchers estimate the ecological impact of their work, a
sustainability tracker has been implemented. It is based on the Python
library ‘codecarbon’ by Courty et al. (2023). The tracker estimates
energy consumption for CPUs, GPUs, and RAM during training, and
calculates CO₂ emissions. This value is based on the energy mix in the
country where the computer is located.

## PyTorch as Machine Learning Framework

The core machine learning framework of this package is PyTorch, which
provides broad support for graphics processing units (GPUs) to
accelerate computations, access to new and unique model architectures,
and high compatibility of models across different versions of the
framework.

## Model Life Cycle

Reproducibility is essential for research. Therefore, ensuring that
models that have already been trained work with future versions of this
package is a top priority from version 1.0.0 onwards.

## Classification Tasks

### Transforming Texts into Numbers

Classification tasks require the transformation of raw texts into a
representation with numbers. For this step, *aifeducation* supports new
approaches such as modernBERT (Warner et al. 2024), MPNet (Song et
al. 2020), BERT (Devlin et al. 2019), RoBERTa (Liu et al. 2019), and
Funnel-Transformer (Dai et al. 2020).

*aifeducation* supports the use of pre-trained transformer models
provided by [Hugging Face](https://huggingface.co/) and the creation of
new transformers. This allows educators and researchers to develop
specialised, domain-specific models. For details about the configuration
of a new model, see [04 Model configuration and
training](https://fberding.github.io/aifeducation/articles/model_configuration.html).

The package supports the analysis of long texts. Depending on the
method, long texts are either transformed into vectors immediately or
split into several chunks if they are too long, resulting in a sequence
of vectors.

### Training AI under Challenging Conditions

In the second step of a classification task, aifeducation integrates
important statistical and mathematical methods to address the main
challenges of applying AI in educational and social sciences. These are:

- **digital data availability:** In the social and educational sciences,
  data is often only available in handwritten form. For instance,
  students at school or university frequently complete tasks by creating
  handwritten documents. This means that educators and researchers must
  first transform analogue data into digital form, a process that
  involves human action. This makes data generation expensive and
  time-consuming, resulting in *small datasets*.
- **high privacy policy standards:** Furthermore, in the social and
  educational sciences, data often refers to humans and/or their
  actions. These kinds of data are protected by privacy policies in many
  countries, which limit access to and usage of data and result in
  *small data sets*.
- **long research tradition:** The educational and social sciences have
  a long tradition of generating insights into social phenomena, as well
  as learning and teaching. These insights must be incorporated into AI
  applications (e.g. Luan et al. 2020; Wong et al. 2019). Supervised
  machine learning is therefore a very important technology, providing a
  link between educational and social theories or models and machine
  learning (Berding et al. 2022). However, this kind of machine learning
  requires humans to generate valid data sets for the training process,
  resulting in *small data sets*.
- **complex constructs:** Compared to classification tasks, such as
  differentiating between a ‘good’ or ‘bad’ movie review, constructs in
  the educational and social sciences are more complex. Some
  motivational psychology research instruments, for example, require
  personal motives to be inferred from written essays (Gruber &
  Kreuzpointner 2013). Reliably and validly interpreting this kind of
  information requires highly qualified human raters, which makes data
  generation expensive. This also limits the size of a dataset.
- **imbalanced data:** Finally, as several empirical studies have shown
  (Bloemen 2011; Stütz et al. 2022), data in the educational and social
  sciences often occurs in imbalanced patterns. This means that some
  categories or characteristics of a dataset have very high absolute
  frequencies compared to others. During AI training, imbalance causes
  algorithms to focus on and prioritise categories and characteristics
  with high absolute frequencies, increasing the risk of missing
  categories/characteristics with low frequencies (Haixiang et
  al. 2017). This can cause AI systems to favour certain groups of
  people or types of material, make false recommendations, and draw
  incorrect conclusions. It can also cause them to miss rare categories
  or characteristics.

To address the issue of imbalanced datasets, the package incorporates
the *Synthetic Minority Oversampling Technique* (SMOTE) into the
learning process. The K-Nearest Neighbour Oversampling Approach (KNNOR),
which was developed by Islam et al. (2022), is currently available in
fast C++. Compared to other techniques, this approach has been shown to
perform well across different tasks and datasets (Islam et al. 2022).

To address the issue of limited data sets, AI training loops incorporate
a technique known as *pseudo-labelling* (e.g. Lee 2013).
Pseudo-labelling is a supervised learning technique. More specifically,
educators and researchers rate part of a dataset, which is then used to
train the AI. The rest of the data is not processed by humans. Instead,
the AI uses this data to learn independently. Therefore, educators and
researchers only need to provide additional data for the AI’s learning
process rather than coding it themselves. This makes it possible to add
more data to the training process while reducing labour costs.

### Evaluating Performance

In machine learning, classification tasks can be compared to the
empirical method of *content analysis* used in the social sciences. This
method has a long research tradition, as well as an ongoing discussion
about how to evaluate the reliability and validity of the data it
generates. To establish a connection with this research tradition and
provide educators and educational and social researchers with
performance measures with which they are more familiar, every AI trained
with this package is evaluated using the following measures and
concepts:

- Iota Concept of the Second Generation (Berding & Pargmann 2022).
- Krippendorff’s Alpha (Krippendorff 2019).
- Percentage Agreement.
- Gwet’s AC1/AC2 (Gwet 2014).
- Kendall’s coefficient of concordance W.
- Cohen’s Kappa unweighted (Cohen 1960).
- Cohen’s Kappa with equal weights (Cohen 1968).
- Cohen’s Kappa with squared weights (Cohen 1968).
- Fleiss’ Kappa for multiple raters without exact estimation (Fleiss
  1971).

In addition, some traditional measures from machine learning literature
are also available:

- Precision
- Recall
- F1-Score

## Sharing Trained AI

As the package is based on PyTorch and the Transformer library, any
trained AI model can be shared with other educators and researchers. The
package not only supports the straightforward use of pre-trained AI
within *R*, but also provides the option to export trained AI to other
environments.

To use a pre-trained AI model for classification purposes, you only need
the classifier itself and the corresponding text embedding model. Either
use AI for Education Studio or simply load both to R and start making
predictions. Vignette [02 Using the graphical user interface
Aifeducation -
Studio](https://fberding.github.io/aifeducation/articles/gui_aife_studio.html)
describes how to use the user interface. Vignette [03 Using R
syntax](https://fberding.github.io/aifeducation/articles/classification_tasks.html)
describes how to save and load the objects with *R* syntax. In vignette
[05 Sharing and Using Trained
AI/Models](https://fberding.github.io/aifeducation/articles/sharing_and_publishing.html)
you can find a detailed guide on how to document and share your models.

## Tutorial and Guides

- [01 Get
  Started](https://fberding.github.io/aifeducation/articles/aifeducation.html):
  Installation and configuration of the package.
- [02 Using the graphical user interface Aifeducation -
  Studio](https://fberding.github.io/aifeducation/articles/gui_aife_studio.html):
  Introduction graphical user interface *Aifeducation Studio*.
- [03 Using R
  syntax](https://fberding.github.io/aifeducation/articles/classification_tasks.html):
  A short introduction into using the package with *R* syntax with
  examples for classification tasks.
- [04 Model configuration and
  training](https://fberding.github.io/aifeducation/articles/model_configuration.html):
  Summary of some studies for finding a good configuration for a model.
- [05 Sharing and Using Trained
  AI/Models](https://fberding.github.io/aifeducation/articles/sharing_and_publishing.html):
  Guidance on how to share models.

## References

Berding, F., & Pargmann, J. (2022). Iota Reliability Concept of the
Second Generation. Berlin: Logos. <https://doi.org/10.30819/5581>

Berding, F., Riebenbauer, E., Stütz, S., Jahncke, H., Slopinski, A., &
Rebmann, K. (2022). Performance and Configuration of Artificial
Intelligence in Educational Settings.: Introducing a New Reliability
Concept Based on Content Analysis. Frontiers in Education, 1-21.
<https://doi.org/10.3389/feduc.2022.818365>

Bloemen, A. (2011). Lernaufgaben in Schulbüchern der Wirtschaftslehre:
Analyse, Konstruktion und Evaluation von Lernaufgaben für die Lernfelder
industrieller Geschäftsprozesse. Hampp.

Cohen, J (1968). Weighted kappa: Nominal scale agreement with provision
for scaled disagreement or partial credit. Psychological Bulletin,
70(4), 213–220. <https://doi.org/10.1037/h0026256>

Cohen, J (1960). A Coefficient of Agreement for Nominal Scales.
Educational and Psychological Measurement, 20(1), 37–46.
<https://doi.org/10.1177/001316446002000104>

Courty, B., Schmidt, V., Goyal-Kamal, Coutarel, M., Feld, B., Lecourt,
J., & … (2023). mlco2/codecarbon: v2.2.7.
<https://doi.org/10.5281/zenodo.8181237>

Dai, Z., Lai, G., Yang, Y. & Le, Q. V. (2020). Funnel-Transformer:
Filtering out Sequential Redundancy for Efficient Language Processing.
<https://doi.org/10.48550/arXiv.2006.03236>

Devlin, J., Chang, M.‑W., Lee, K., & Toutanova, K. (2019). BERT:
Pre-training of Deep Bidirectional Transformers for Language
Understanding. In J. Burstein, C. Doran, & T. Solorio (Eds.),
Proceedings of the 2019 Conference of the North (pp. 4171–4186).
Association for Computational Linguistics.
<https://doi.org/10.18653/v1/N19-1423>

Fleiss, J. L. (1971). Measuring nominal scale agreement among many
raters. Psychological Bulletin, 76(5), 378–382.
<https://doi.org/10.1037/h0031619>

Gruber, N., & Kreuzpointner, L. (2013). Measuring the reliability of
picture story exercises like the TAT. PloS One, 8(11), e79450.
<https://doi.org/10.1371/journal.pone.0079450>

Gwet, K. L. (2014). Handbook of inter-rater reliability: The definitive
guide to measuring the extent of agreement among raters (Fourth
edition). STATAXIS.

Haixiang, G., Yijing, L., Shang, J., Mingyun, G., Yuanyue, H., & Bing,
G. (2017). Learning from class-imbalanced data: Review of methods and
applications. Expert Systems with Applications, 73, 220–239.
<https://doi.org/10.1016/j.eswa.2016.12.035>

He, P., Liu, X., Gao, J. & Chen, W. (2020). DeBERTa: Decoding-enhanced
BERT with Disentangled Attention.
<https://doi.org/10.48550/arXiv.2006.03654>

Islam, A., Belhaouari, S. B., Rehman, A. U. & Bensmail, H. (2022).
KNNOR: An oversampling technique for imbalanced datasets. Applied Soft
Computing, 115, 108288. <https://doi.org/10.1016/j.asoc.2021.108288>

Krippendorff, K. (2019). Content Analysis: An Introduction to Its
Methodology (4th Ed.). SAGE.

Lee, D.‑H. (2013). Pseudo-Label: The Simple and Efficient
Semi-Supervised Learning Method for Deep Neural Networks. CML 2013
Workshop: Challenges in Representation Learning.

Liu, Y., Ott, M., Goyal, N., Du, J., Joshi, M., Chen, D., Levy, O.,
Lewis, M., Zettlemoyer, L., & Stoyanov, V. (2019). RoBERTa: A Robustly
Optimized BERT Pretraining Approach.
<https://doi.org/10.48550/arXiv.1907.11692>

Luan, H., Geczy, P., Lai, H., Gobert, J., Yang, S. J. H., Ogata, H.,
Baltes, J., Guerra, R., Li, P., & Tsai, C.‑C. (2020). Challenges and
Future Directions of Big Data and Artificial Intelligence in Education.
Frontiers in Psychology, 11, 1–11.
<https://doi.org/10.3389/fpsyg.2020.580820>

Song, K., Tan, X., Qin, T., Lu, J. & Liu, T.‑Y. (2020). MPNet: Masked
and Permuted Pre-training for Language Understanding.
<https://doi.org/10.48550/arXiv.2004.09297>

Stütz, S., Berding, F., Reincke, S., & Scheper, L. (2022).
Characteristics of learning tasks in accounting textbooks: an AI
assisted analysis. Empirical Research in Vocational Education and
Training, 14(1). <https://doi.org/10.1186/s40461-022-00138-2>

Warner, B., Chaffin, A., Clavié, B., Weller, O., Hallström, O.,
Taghadouini, S., Gallagher, A., Biswas, R., Ladhak, F., Aarsen, T.,
Cooper, N., Adams, G., Howard, J. & Poli, I. (2024). Smarter, Better,
Faster, Longer: A Modern Bidirectional Encoder for Fast, Memory
Efficient, and Long Context Finetuning and Inference.
<https://doi.org/10.48550/arXiv.2412.13663>

Wong, J., Baars, M., Koning, B. B. de, van der Zee, T., Davis, D.,
Khalil, M., Houben, G.‑J., & Paas, F. (2019). Educational Theories and
Learning Analytics: From Data to Knowledge. In D. Ifenthaler, D.-K. Mah,
& J. Y.-K. Yau (Eds.), Utilizing Learning Analytics to Support Study
Success (pp. 3–25). Springer.
<https://doi.org/10.1007/978-3-319-64792-0_1>
