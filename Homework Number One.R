# Gary Gregg, INFO 474, Assignment #1
#
# 27 January, 2017: Added base 10 log transformation of MIC values, and
# adjusted title to reflect the change.

# The 'lattice' library is required for a levelplot.
library("lattice")

# Declare the antibiotic vector.
antibiotic <- c("Penicillin", "Streptomycin", "Neomycin")

# Declare the bacteria vector.
bacteria <- c("Aerobacter aerogenes",
              "Brucella abortus",
              "Brucella anthracis",
              "Diplococcus pneumoniae",
              "Escherichia coli",
              "Klebsiella pneumoniae",
              "Mycobacterium tuberculosis",
              "Proteus vulgaris",
              "Pseudomonas aeruginosa",
              "Salmonella (Eberthella) typohsa",
              "Salmonella schottmuelleri",
              "Staphylococcus albus",
              "Staphylococcus aureus",
              "Streptococcus fecalis",
              "Streptococcus hemolyticus",
              "Streptococcus viridans")

# Declare the gram-staining vector.
gram_staining <- c(FALSE,
                   FALSE,
                   TRUE,
                   TRUE,
                   FALSE,
                   FALSE,
                   FALSE,
                   FALSE,
                   FALSE,
                   FALSE,
                   FALSE,
                   TRUE,
                   TRUE,
                   TRUE,
                   TRUE,
                   TRUE)

# The Minimum Inhibitory Concentration (MIC) vector is reversed.
mic <- c(
  1.6, 1., 870.,
  0.02, 2., 1.,
  0.007, 0.01, 0.001,
  10., 11., 0.005,
  0.1, 0.4, 100.,
  1., 1.2, 850.,
  2., 5., 800.,
  0.1, 0.1, 3.,
  0.4, 2., 850.,
  0.008, 0.4, 1.,
  0.09, 0.8, 10.,
  0.001, 0.1, 0.007,
  0.001, 0.03, 0.03,
  0.1, 1., 1.,
  10., 14., 0.001,
  40., 10., 0.005
)

# Expand the data into a grid, with domain as the antibiotic, and range as the bacteria.
data <- expand.grid(X=antibiotic, Y=rev(bacteria))
data$Z <- rev(mic)

# Reverse the gram-staining vector only once.
rev_gram_staining <- rev(gram_staining)

# Set graph parameters, and create the levelplot.
par(mar=c(3,4,2,2))
levelplot(log10(Z) ~ X*Y,
          data=data,
          xlab="Antibiotic",
          ylab="Bacteria (Gram Stained in Bold Dark Blue)",
          panel = function(...){
            panel.levelplot(...)
            panel.abline(h = seq(1.5, 15.5, 1.))
            panel.abline(v = seq(1.5, 2.5, 1.))
          },
          scales=list(y=list(col=ifelse(rev_gram_staining, "darkblue", "black"),
                             fontface=ifelse(rev_gram_staining, "bold", "plain"))),
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          main="Minimum Inhibitory Concentration (MIC) in Base 10 Log Scale")
