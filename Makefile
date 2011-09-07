MODULE = vault
VERSION = 2.0.0

GEN = gen

SRC_PROD = src/prod
SRC_TEST = src/test
SRC_DEMO = src/demo

CLS_PROD = gen/classes/prod
CLS_TEST = gen/classes/test
CLS_DEMO = gen/classes/demo

CP_BASE = lib/run/\*:lib/run/scalaz/\*:lib/test/\*
CP_PROD = ${CP_BASE}:${CLS_PROD}
CP_TEST = ${CP_PROD}:${CLS_TEST}

DIST = ${GEN}/dist

JAR = ${DIST}/${MODULE}.jar
JAR_SRC = ${DIST}/${MODULE}-src.jar

TAR = ${DIST}/${MODULE}-${VERSION}.tar.gz

HASH = ${ETC}/sha1
HASH_JAR = ${JAR}.sha1
HASH_TAR = ${TAR}.sha1

LICENSES = etc/licenses
MANIFEST = etc/MANIFEST.MF
DIST_MANIFEST = ${GEN}/MANIFEST.MF
TAR_IMAGE = ${GEN}/image/${MODULE}-${VERSION}
RELEASE_DIR = ${GEN}/release/${VERSION}

DIRECTORIES = ${GEN} ${GEN}/tmp ${CLS_DEMO} ${CLS_PROD} ${CLS_TEST} ${DIST} ${TAR_IMAGE} ${TAR_IMAGE}/lib  ${RELEASE_DIR} ${DEMO_TARGET}


.PHONY: clean dist compile size repl 

default: test dist

compile: clean ${CLS_PROD} ${CLS_TEST} ${CLS_DEMO}
	find ${SRC_PROD} -name "*.scala" | xargs -s 30000 scalac -classpath ${CP_BASE} -d ${CLS_PROD} 
	find ${SRC_DEMO} -name "*.scala" | xargs -s 30000 scalac -classpath ${CP_PROD} -d ${CLS_DEMO} 
	find ${SRC_TEST} -name "*.scala" | xargs -s 30000 scalac -classpath ${CP_PROD} -d ${CLS_TEST} 

test: compile
	scala -cp ${CP_TEST} org.scalatest.tools.Runner -p ${CLS_TEST} -oDFW 

${JAR}: compile ${DIST_MANIFEST} ${DIST}
	jar cfm ${JAR} ${DIST_MANIFEST} -C ${CLS_PROD} .

${JAR_SRC}: ${DIST}
	jar cf ${JAR_SRC} -C ${SRC_PROD} .

${TAR}: ${JAR} ${JAR_SRC} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${DEMO_TARGET}
	cp -r ${SRC_DEMO} ${TAR_IMAGE}/. && \
	cp lib/run/*.jar lib/run/scalaz/*.jar ${TAR_IMAGE}/lib && \
	cp lib/run/scalaz/*.jar ${DIST}/ && \
	cp ${JAR} ${JAR_SRC} ${TAR_IMAGE} && \
	cp README ${TAR_IMAGE} && \
	cp -r ${LICENSES} ${TAR_IMAGE} && \
	tar cfz ${TAR} -C ${GEN}/image .

dist: clean ${TAR}


${HASH_JAR}:
	${HASH} ${JAR} > ${HASH_JAR}

${HASH_TAR}:
	${HASH} ${TAR} > ${HASH_TAR}

${DIST_MANIFEST}: ${GEN}
	sed -e 's/VERSION/${VERSION}/' ${MANIFEST} > ${DIST_MANIFEST}

repl: compile
	scala -classpath ${CP_BASE}:${CLS_PROD}:${CLS_TEST}

size: 
	find ${SRC_PROD} -name "*.scala" | xargs wc | sort -n

${DIRECTORIES}:
	mkdir -p $@

clean:
	rm -rf ${GEN}; find . -name "*~" -o -name "*.core" -print0 | xargs -0 rm -f
