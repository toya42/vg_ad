.PHONY: docker-build docker-shell docker-dryrun setup dryrun docs site check coverage

docker-build:
	docker build -t vg_ad .

docker-shell:
	docker run --rm -it -v $(PWD):/workspace/vg_ad vg_ad bash

docker-dryrun:
	docker run --rm -v $(PWD):/workspace/vg_ad vg_ad Rscript scripts/run_dryrun.R --sensor_id VG09 --date 2023-01-01

setup:
	Rscript scripts/setup_env.R

dryrun:
        Rscript scripts/run_dryrun.R --sensor_id VG09 --date 2023-01-01

docs:
        Rscript scripts/document.R

site:
        R -q -e 'pkgdown::build_site()'

check:
        R -q -e 'devtools::check()'

coverage:
        R -q -e 'covr::report()'
