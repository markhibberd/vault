var cleanDirs = ['lib'];

var licenses = "lib/run/licenses/";

var scalazV = "2012-04-04_11-17-31";

var dependencies = [
    // ==== Ephox - Production ====
    // ==== Third Party - Production - REQUIRES LICENSE ====
    { name : "scala",
        repository : "thirdpartyrepo",
        version : "2.9.1",
        source : "scala-2.9.1.final.zip",
        targets : [
            { name : "scala-2.9.1.final/doc/LICENSE", path : licenses + "scala" },
            { name : "scala-2.9.1.final/lib/scala-library.jar", path : "lib" },
            { name : "scala-2.9.1.final/src/scala-library-src.jar", path : "lib" },
            { name : "scala-2.9.1.final/*", path : "lib/tools/scala" }
        ],
        executables : [
            "lib/tools/scala/bin/scala",
            "lib/tools/scala/bin/scalac",
            "lib/tools/scala/bin/fsc",
            "lib/tools/scala/bin/scaladoc",
            "lib/tools/scala/bin/scalap"
        ]
    },
    { name: "scalaz",
        repository: "buildrepo2",
        version : "7/" + scalazV,
        source: "./",
        targets: [
            { name: "*.jar", path: "lib" }
        ]
    },
    { name: "scalaz",
        repository: "buildrepo2",
        version : "7/" + scalazV,
        source: "LICENCE",
        targets: [
            { name: "LICENCE", path: licenses + "scalaz" }
        ]
    },

    // ==== Third Party - Testing / Compile ====
    { name: "db2",
        repository: "thirdpartyrepo",
        source: "db2jcc.jar",
        targets: [
            { name: "db2jcc.jar", path: "lib" }
        ]
    },
    { name: "db2",
        repository: "thirdpartyrepo",
        source: "db2jcc_license_cisuz.jar",
        targets: [
            { name: "db2jcc_license_cisuz.jar", path: "lib" }
        ]
    },
    { name : "hsqldb",
        repository : "thirdpartyrepo",
        source : "hsqldb*.zip",
        version:    "2.0.0/2.0.0.0",
        targets : [
            {name : "hsqldb/lib/hsqldb.jar", path : "lib"}
        ]
    },
    { name : "jtds",
        repository : "thirdpartyrepo",
        source : "jtds-1.2.5.jar",
        version:    "1.2.5",
        targets : [
            {name : "jtds-1.2.5.jar", path : "lib"}
        ]
    },
    { name: "oracle",
        repository: "thirdpartyrepo",
        version: "5",
        source: "ojdbc5.jar",
        targets: [
            { name: "ojdbc5.jar", path: "lib" }
        ]
    },
    { name : "scalacheck",
        repository : "thirdpartyrepo",
        version : "2.9.0-1.9",
        source : "scalacheck_2.9.0-1.9.jar",
        targets : [
            { name : "scalacheck_2.9.0-1.9.jar", path : "lib" }
        ]
    },
    { name : "scalacheck",
        repository : "thirdpartyrepo",
        version : "2.9.0-1.9",
        source : "scalacheck_2.9.0-1.9-sources.jar",
        targets : [
            { name : "scalacheck_2.9.0-1.9-sources.jar", path : "lib" }
        ]
    },
    { name : "scalatest",
        repository : "thirdpartyrepo",
        version : "2.9.1-1.6.1",
        source : "scalatest_2.9.1-1.6.1.jar",
        targets : [
            { name : "scalatest_2.9.1-1.6.1.jar", path : "lib" }
        ]
    },
    { name : "specs2",
        repository : "thirdpartyrepo",
        version : "2.9.1-1.6",
        source : "specs2_2.9.1-1.6.jar",
        targets : [
            { name : "specs2_2.9.1-1.6.jar", path : "lib" }
        ]
    },
    { name : "specs2",
        repository : "thirdpartyrepo",
        version : "2.9.1-1.6",
        source : "specs2_2.9.1-1.6-sources.jar",
        targets : [
            { name : "specs2_2.9.1-1.6-sources.jar", path : "lib" }
        ]
    },
    { name : "specs2",
        repository : "thirdpartyrepo",
        version : "2.9.1-1.6",
        source : "specs2-scalaz-core_2.9.1-6.0.1.jar",
        targets : [
            { name : "specs2-scalaz-core_2.9.1-6.0.1.jar", path : "lib" }
        ]
    }
];