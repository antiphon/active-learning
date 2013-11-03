# Ideas on active learning

## Possible application senarios for our idea

* Preference matching
 * http://www.cs.toronto.edu/~zemel/documents/al4matching_icml.pdf
 * http://arxiv.org/abs/1202.3706

* Interactive data visualization
 * http://jmlr.org/proceedings/papers/v31/iwata13a.pdf

* Information retrieval

## Publishing forum
Machine Learning -journal

## Modeling ideas, labeling example
Main idea: User not an oracle.

Two ways to model:
1. User inputs probability vectors
2. User's input is not reliable

(And of course 1+2: user's probability vector is not reliable).

### User inputs probability vectors

Observations are $y$ simplices, $\sum y_i=1$. Obvious candidate models are Dirichlet distribution,
 Dirichlet process.

Questions: 
1. How do regression on Dirichlet parameters $y_i | x_i \sim Dir(\alpha(x_i))$
2. 