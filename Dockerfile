FROM continuumio/miniconda3

COPY . . 

RUN conda env create -f environment.yml

RUN pip install -e .

ENTRYPOINT [ "conda", "run", "-n", "foobar-dashboard", "R", "-e", "shiny::runApp('src/visualization/shiny/', port = 7890)" ]
