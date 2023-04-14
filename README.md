
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aifeducation

<!-- badges: start -->
<!-- badges: end -->

The R package *Artificial Intelligence for Education (aifeducation)* is
designed for the special needs of educators, educational researchers,
and social researchers. The package supports the application of
Artificial Intelligence (AI) for Natural Language Processing tasks such
as text embedding, classification, and question answering under the
special conditions of educational and social sciences. These are:

- **digital data availability:** In educational and social science data
  is often available only in a hand written form. For example, students
  and pupils often solve tasks in schools or universities by creating
  hand written documents. Thus, educators and researchers have first to
  transform textual data into a digital form involving human actions.
  This makes data generation financial expensive and time consuming
  leading to *small data sets*.
- **high privacy policies standards:** Furthermore, in educational and
  social science data often refers to humans and/or human actions. These
  kind of data is protected by privacy policies in many countries
  limiting the access and the usage of data which in turn results in
  *small data sets*.  
- **long research tradition:** Educational and social science have a
  long research tradition generating insights into social phenomenon as
  well as learning and teaching. These insights have to be incorporated
  into applications of AI (e.g., Luan et al. 2020; Wong et al. 2019).
  This makes supervised machine learning a very important technique
  since it provides a link between educational and social theories and
  models on the one hand and machine learning on the other hand (Berding
  et al. 2022). However, this kind of machine learning requires humans
  for generating a valid data set for training AI leading to *small data
  sets*.
- **complex constructs:** Compared to classification tasks where an AI
  has to differentiate between a ‘good’ or ‘bad’ movie review constructs
  in educational and social science are more complex. For example, some
  research instruments in motivation psychology require to infer
  personal motives from written essays (e.g., Gruber & Kreuzpointner
  2013). A reliable and valid interpretation of this kind of information
  requires well qualified human raters making data generation expensive.
  This also *limits the size of a data set*.
- **imbalanced data:** Finally, data in educational and social science
  often occurs in an imbalanced pattern as several empirical studies
  show (Bloemen 2011, Stütz et al. 2022). Imbalanced means that some
  categories or characteristics of a data set have a very high absolute
  frequencies compared to other categories and characteristics.
  Imbalance during training AI guides algorithms to focus and to
  prioritize the categories and characteristics with high absolute
  frequencies increasing the risk to miss categories/characteristics
  with low frequencies (Haixiang et al. 2017). This can lead AI to
  prefer special groups of persons/materials, to imply false
  recommendations and conclusions, and to miss rare categories or
  characteristics.

Currently the package focuses on classification tasks which can be used
for diagnosing characteristics of learners from written materials or for
estimating the properties of learning and teaching materials. In the
future more tasks will be implemented.

## Installation

You can install the development version of aifeducation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FBerding/aifeducation")
```

## Classification Tasks

### Transforming Texts into Numbers

Classification tasks require the transformation of raw texts into a
representation with numbers. For this step in a classification task
*aifeducation* supports newer approaches such as BERT (Devlin et
al. 2019) and older approaches such as GlobalVectors (Pennington, Socher
& Manning 2014) or Latent Dirichlet Allocation/Topic Modeling. Other
newer approaches such as ROBERTA (Liu et al. 2019) or Longformer
(Beltagy, Peters & Cohan 2020) are planned for the future.

*aifeducation* supports the usage of pre-trained transformer models
provided by [Huggingface](https://huggingface.co/) and the creation of
new transformers allowing educators and researchers to development
specialized and domain-specific models.

Package supports analysis of long texts. Depending on the method long
texts are transformed into vectors at once or by splitting long texts
into several chunks which result in a sequence of vectors.

### Training AI under Challenging Conditions

For the second step within a classification task *aifeducation*
integrates some important statistical and mathematical methods for
dealing with the main challenges in educational and social sciences for
applying AI.

In order to deal with the problem of imbalanced data sets package
integrates *Synthetic Minority Oversampling Technique* into the learning
process. Currently the *Basic Synthetic Minority Oversampling
Technique*, *Density-Bases Synthetic Minority Oversampling Technique*,
and *Adaptive Synthetic Sampling Approach for Imbalanced Learning* are
implemented via the *R* package smotefamiliy.

In order to address the problem of small data sets training loops of AI
integrate *Pseudo Labeling*(e.g., Lee 2013). Pseudo Labeling is a
technique which can be used for supervised learning. That is, educators
and researchers rate a part of a data set and train AI with this part of
data. The remaining part of data is not processed by humans. Instead, AI
uses this part of data to learn on its own. Thus, educators and
researchers do only have to provide additional data for learning without
working on it. This offers the possibility to add more data to the
training and to reduce personal costs.

### Evaluating Performance

Classification tasks in machine learning are comparable to the empirical
method of *content analysis* from social science. This method looks back
to a long research tradition and a long discussion about how to evaluate
the reliability and validity of generated data. In order to provide a
link to this research tradition and in order to provide educators as
well as educational and social researchers  
with performance measures they are more familiar with every AI trained
with this packages is evaluated with the following measures and
concepts:

- Iota Concept of the Second Generation (Berding & Pargmann 2022)
- Krippendorff’s Alpha (Krippendorff 2019)
- Percentage Agreement
- Gwet’s AC1/AC2 (Gwet 2014)
- Kendall’s coefficient of concordance W
- Cohen’s Kappa with equal weights
- Fleiss’ Kappa for multiple raters with exact estimation
- Light’s Kappa for multiple raters

## Sharing Trained AI

Since the package is based on keras, tensorflow, and the transformer
libraries every trained AI can be shared with other educators and
researchers. The package supports an easy use of pre-trained AI within
*R* but also provides the possibility to export trained AI to other
environments.

## Tutorial and Guides

A guide how to install and configure the package can be found via [Get
started](aifeducation.html). A short introduction in using the package
for classification tasks can be found in the vignette [02 clasification
tasks](classification_tasks.html)

## References

Beltagy, I., Peters, M. E., & Cohan, A. (2020). Longformer: The
Long-Document Transformer. <https://doi.org/10.48550/arXiv.2004.05150>

Berding, F., & Pargmann, J. (2022). Iota Reliability Concept of the
Second Generation. Logos Verlag Berlin. <https://doi.org/10.30819/5581>

Berding, F., Riebenbauer, E., Stütz, S., Jahncke, H., Slopinski, A., &
Rebmann, K. (2022). Performance and Configuration of Artificial
Intelligence in Educational Settings.: Introducing a New Reliability
Concept Based on Content Analysis. Frontiers in Education, 1–21.
<https://doi.org/10.3389/feduc.2022.818365>

Bloemen, A. (2011). Lernaufgaben in Schulbüchern der Wirtschaftslehre:
Analyse, Konstruktion und Evaluation von Lernaufgaben für die Lernfelder
industrieller Geschäftsprozesse. Hampp.

Devlin, J., Chang, M.‑W., Lee, K., & Toutanova, K. (2019). BERT:
Pre-training of Deep Bidirectional Transformers for Language
Understanding. In J. Burstein, C. Doran, & T. Solorio (Eds.),
Proceedings of the 2019 Conference of the North (pp. 4171–4186).
Association for Computational Linguistics.
<https://doi.org/10.18653/v1/N19-1423>

Gruber, N., & Kreuzpointner, L. (2013). Measuring the reliability of
picture story exercises like the TAT. PloS One, 8(11), e79450.
<https://doi.org/10.1371/journal.pone.0079450>

Gwet, K. L. (2014). Handbook of inter-rater reliability: The definitive
guide to measuring the extent of agreement among raters (Fourth
edition). Advances Analytics LLC.

Haixiang, G., Yijing, L., Shang, J., Mingyun, G., Yuanyue, H., & Bing,
G. (2017). Learning from class-imbalanced data: Review of methods and
applications. Expert Systems with Applications, 73, 220–239.
<https://doi.org/10.1016/j.eswa.2016.12.035>

Krippendorff, K. (2019). Content Analysis: An Introduction to Its
Methodology (4th Ed.). SAGE.

Lee, D.‑H. (2013). Pseudo-Label : The Simple and Efficient
Semi-Supervised Learning Method for Deep Neural Networks. CML 2013
Workshop : Challenges in RepresentationLearning.
<https://www.researchgate.net/publication/280581078_Pseudo-Label_The_Simple_and_Efficient_Semi-Supervised_Learning_Method_for_Deep_Neural_Networks>

Liu, Y., Ott, M., Goyal, N., Du, J., Joshi, M., Chen, D., Levy, O.,
Lewis, M., Zettlemoyer, L., & Stoyanov, V. (2019). RoBERTa: A Robustly
Optimized BERT Pretraining Approach.
<https://doi.org/10.48550/arXiv.1907.11692>

Luan, H., Geczy, P., Lai, H., Gobert, J., Yang, S. J. H., Ogata, H.,
Baltes, J., Guerra, R., Li, P., & Tsai, C.‑C. (2020). Challenges and
Future Directions of Big Data and Artificial Intelligence in Education.
Frontiers in Psychology, 11, 1–11.
<https://doi.org/10.3389/fpsyg.2020.580820>

Pennington, J., Socher, R., & Manning, C. D. (2014). GloVe: Global
Vectors for Word Representation. Proceedings of the 2014 Conference on
Empirical Methods in Natural Language Processing.
<https://aclanthology.org/D14-1162.pdf>

Stütz, S., Berding, F., Reincke, S., & Scheper, L. (2022).
Characteristics of learning tasks in accounting textbooks: an AI
assisted analysis. Empirical Research in Vocational Education and
Training, 14(1). <https://doi.org/10.1186/s40461-022-00138-2>

Wong, J., Baars, M., Koning, B. B. de, van der Zee, T., Davis, D.,
Khalil, M., Houben, G.‑J., & Paas, F. (2019). Educational Theories and
Learning Analytics: From Data to Knowledge. In D. Ifenthaler, D.-K. Mah,
& J. Y.-K. Yau (Eds.), Utilizing Learning Analytics to Support Study
Success (pp. 3–25). Springer International Publishing.
<https://doi.org/10.1007/978-3-319-64792-0_1>
