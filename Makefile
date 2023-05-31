# This makefile provides commands that ease the rigamarole around the spells
# to cast to docker-up this thing so we can deploy it via ShinyProxy
#
# To build an image for use in package development:
#
#    $ make build
#
# Launch the shiny app:
#
#    $ make shiny
#
# To push and pull images to our aws container registery, you need to be
# authenticated with our account. The code here assumes you have the
# appropriate environment variables set, like `AWS_ACCESS_KEY_ID`, and
# `AWS_SECRET_ACCESS_KEY` (and optionally `AWS_SESSION_TOKEN`, if necessary).
# Once that is setup, you can "credential up" with the registry like so
#
#    $ make ecr-login
#
# You only have to `ecr-login` once per session (more or less), but it
# doesn't hurt if you do it multiple times. Therefore, the `ecr-target` is
# listed as a dependencyof the `push` and `pull` targets.
#
# Pushing the built image to our internal registry:
#
#    $ make ecr-push
#
# Pulling the image from the aws registry to the local computer
#
#    $ make ecr-pull
#
# By default, we are working with the "release" version, but we can work
# on the "devel" version by passing variables to the make command like so:
#
#    $ make rstudio version=devel
#
# The following resources were used as reference while developing this Makefile
#     https://github.com/fbreitwieser/pavian
#     https://stackoverflow.com/a/2826178/83761 [to allow defined variables at the command line]

version?=latest # `devel` or `latest`
rport?=8080# rstudio port
sport?=8888# shiny port
imagename=datascience/omicsdashboard
datadir?=${data_dir}
tag=${imagename}:${version}
rev=$(shell git rev-parse HEAD | cut -c1-7)

# registry setup -------------------------------------------------------------
aws_account?=1234567890# set this

PROFILE1_ID=12345# aws sts get-caller-identity --query Account --output text
PROFILE1=profile1-user

PROFILE2_ID=54321# aws sts get-caller-identity --query Account --output text
PROFILE2=profile2-user

ifeq ($(aws_account),profile1)
	aws_profile=${PROFILE1}
	aws_account_id=${PROFILE1_ID}
else ifeq ($(aws_account),profile2)
	aws_profile=${PROFILE2}
	aws_account_id=${PROFILE2_ID}
endif

aws_region=us-west-2
aws_ecr_url=${aws_account_id}.dkr.ecr.${aws_region}.amazonaws.com
aws_image_name=${aws_ecr_url}/${tag}
aws_image_rev=${aws_ecr_url}/${imagename}:${rev}

# requires installation of net-tools (sudo apt install net-tools)
LOCALIP=$(shell ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1' | awk '{print $1}' | tail -n1)
#dockerdir=docker/${version}
dockerdir=shinyproxy
secretfile=${dockerdir}/.env
dockerfile=${dockerdir}/Dockerfile

# docker build customizations -------------------------------------------------
ifdef nocache
cache_directive=--no-cache
else
cache_directive=
endif

# use `make build progress=plain` if you want the verbose details of the build
progress?=auto
#progress?=plain


# This requires a `make ecr-login` first because our dockerfile depends on an
# image in the ecr registry.
#
# Uses the secrets functionality in docker >-= 20.10 to pull the GITHUB_PAT from
# the local environment and make it available as a secret to the Dockerfile.
# The GITHUB_PAT can only be exposed "per layer" (ie. for just one RUN command
# at a time). To create a GITHUB_PAT environment variable during the dobker builc
# process, you will have to read the secrets file from /run/secrets and set it
# to an environment varialbe, like so:
#
#     RUN --mount=type=secret,id=GITHUB_PAT \
#         GITHUB_PAT="$(cat /run/secrets/GITHUB_PAT)" \
#         Rscript -e "BiocManager::install('facilebio/FacileBiocData')
#
# For mor information: https://pythonspeed.com/articles/docker-build-secrets/
# build: ecr-login
build:
	DOCKER_BUILDKIT=1 docker build \
		--progress ${progress} \
		${cache_directive} \
		--secret id=GITHUB_PAT \
		-f ${dockerfile} \
		-t ${tag} \
		${dockerdir}

# useful for dropping into the shell of your container for debugging
inspect:
# you may need these docker args for bspm:
# --security-opt seccomp=unconfined
	docker run --rm -it \
	  -p ${sport}:3838 \
	  --volume ${datadir}:/data \
	  --env data_dir=/data \
	  -it --entrypoint /bin/bash \
	  ${tag}

# rstudio:
# 	docker run --rm -it -d \
# 	  -p ${rport}:8787 \
# 	  --env SHINYPROXY_USERNAME=$(USER) \
# 	  --env DISABLE_AUTH=true \
# 	  -v $(shell pwd):/home/rstudio/yourapp \
# 	  --entrypoint /init \
# 	 ${tag}
# 	echo "Navigate to: http://${LOCALIP}:${rport}"

shiny:
	docker run --rm -it -d \
	  -p ${sport}:3838 \
	  --env SHINYPROXY_USERNAME=$(USER) \
	  --volume ${datadir}:/data \
	  --env data_dir=/data \
 	  ${tag}
	echo "Navigate to: http://${LOCALIP}:${sport}"

# Pushing and pulling images to AWS -------------------------------------------------
# This assumes you are using the v2 of the cli, look here for inspiration:
# https://gist.github.com/miguelmota/c13bd2f5cc5493c82689c40117846571?permalink_comment_id=3782563#gistcomment-3782563

ecr-login:
	aws ecr get-login-password --region ${aws_region} --profile ${aws_profile} | docker login --password-stdin --username AWS ${aws_ecr_url}

ecr-push: ecr-login
	docker tag ${tag} ${aws_image_name}
	docker tag ${tag} ${aws_image_rev}
	docker push ${aws_image_name}
	docker push ${aws_image_rev}

ecr-pull: ecr-login
	docker pull ${aws_image_name}
	docker tag ${aws_image_name} ${tag}
