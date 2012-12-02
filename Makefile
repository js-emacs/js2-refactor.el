ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all:
	${HOME}/.carton/bin/carton exec ${ECUKES} features
