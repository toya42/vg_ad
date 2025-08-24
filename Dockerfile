FROM rocker/verse:4.4.1

WORKDIR /workspace/vg_ad

COPY . /workspace/vg_ad

RUN Rscript scripts/setup_env.R

CMD ["bash"]
