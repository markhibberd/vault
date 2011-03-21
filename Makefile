MODULE = vault
VERSION = 1.0.0

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

DOC_PROD = ${GEN}/doc/prod

XRAY = lib/compile/sxr_2.8.0-0.2.7-SNAPSHOT.jar
XRAY_PROD = ${CLS_PROD}.sxr
XRAY_DEMO = ${CLS_DEMO}.sxr

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

DIRECTORIES = ${GEN} ${GEN}/tmp ${CLS_DEMO} ${CLS_PROD} ${CLS_TEST} ${DIST} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${DOC_PROD} ${RELEASE_DIR} ${TAR_IMAGE}/doc/xray ${DEMO_TARGET}


.PHONY: clean dist doc compile size repl 

default: test dist

compile: clean ${CLS_PROD} ${CLS_TEST} ${CLS_DEMO}
	find ${SRC_PROD} -name "*.scala" | xargs -s 30000 scalac -Xplugin:${XRAY} -P:sxr:base-directory:${SRC_PROD}  -classpath ${CP_BASE} -d ${CLS_PROD} && \
	find ${SRC_DEMO} -name "*.scala" | xargs -s 30000 scalac -Xplugin:${XRAY} -P:sxr:base-directory:${SRC_DEMO}  -classpath ${CP_PROD} -d ${CLS_DEMO} && \
	find ${SRC_TEST} -name "*.scala" | xargs -s 30000 scalac -classpath ${CP_PROD} -d ${CLS_TEST} 

test: compile
	scala -cp ${CP_TEST} org.scalatest.tools.Runner -p ${CLS_TEST} -oDFW 

${JAR}: compile ${DIST_MANIFEST} ${DIST}
	jar cfm ${JAR} ${DIST_MANIFEST} -C ${CLS_PROD} .

${JAR_SRC}: ${DIST}
	jar cf ${JAR_SRC} -C ${SRC_PROD} .

${TAR}: doc ${JAR} ${JAR_SRC} ${TAR_IMAGE} ${TAR_IMAGE}/lib ${TAR_IMAGE}/doc/xray ${DEMO_TARGET}
	cp -r ${DOC_PROD} ${TAR_IMAGE}/doc/api && \
	cp -r ${SRC_DEMO} ${TAR_IMAGE}/. && \
	cp -r ${XRAY_PROD} ${TAR_IMAGE}/doc/xray/prod && \
	cp -r ${XRAY_DEMO} ${TAR_IMAGE}/doc/xray/demo && \
	cp lib/run/*.jar lib/run/scalaz/*.jar ${TAR_IMAGE}/lib && \
	cp lib/run/scalaz/*.jar ${DIST}/ && \
	cp ${JAR} ${JAR_SRC} ${TAR_IMAGE} && \
	cp README ${TAR_IMAGE} && \
	cp -r ${LICENSES} ${TAR_IMAGE} && \
	tar cfz ${TAR} -C ${GEN}/image .

dist: clean ${TAR}

doc: ${DOC_PROD}
	(cd ${SRC_PROD} && \
	find com -name "*.scala" | xargs -s 30000 \
		scaladoc \
			-doc-title "scaladoc for [${MODULE} ${VERSION}]" \
			-doc-version ${VERSION} \
			-classpath ../../lib/run/\*:../../lib/run/scalaz/\*:../../${CLS_PROD} \
			-d ../../${DOC_PROD})

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
