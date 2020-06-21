# machina

A set of analytics tools built with R using the following packages:

- Facebook's [Prophet](https://facebook.github.io/prophet/) package for forecasting.
- Google's [CausalImpact](https://google.github.io/CausalImpact/CausalImpact.html) package for analysing the causal effect of an intervention.

To run locally, open `app.R` in RStudio and press the **Run App** button on the top right just above the code window.

# Deploying with Docker

Build and push the image to docker hub:

```
docker build jeremyyeo/machina .
docker push
```

Pull and run from your server (e.g. an EC2 instance):

```
docker pull jeremyyeo/machina
docker run --rm -p 3838:3838 jeremyyeo/machina
```
