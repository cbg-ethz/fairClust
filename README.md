# Fair Clustering from a Causal Structure Learning Perspective

- The paper discussion Bayesian networks in fairness: [paper](https://arxiv.org/pdf/1907.06430.pdf)
- The paper from which I took the data processing: [paper](https://ojs.aaai.org/index.php/AAAI/article/view/6565)
- "A" recent paper discussing fair clustering: [paper1](https://proceedings.neurips.cc/paper/2019/file/fc192b0c0d270dbf41870a63a8c76c2f-Paper.pdf)

A schematic figure of the method, which needs to be adjusted for:

![Screenshot 2022-07-27 at 13 48 19](https://user-images.githubusercontent.com/38718986/181239603-395c5141-599a-4469-8a61-026550c247fc.png)

Potential usecases: 

- Discovering clusters of discrimination (can we distinguish between clusters in which we have a high/low estimates of discrimination)
- Or cluster the individuals by accounting unfair pathways (e.g. race/gender -> get's the job )

In both cases, the equations need to be modified accordingly (which I have not done yet).
