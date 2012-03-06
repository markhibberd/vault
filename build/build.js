var p = Ent.Project.create("vault", "external");
p.setVersion(2, 0, 0);

function getVersionString() {
    var v = p.version;
    return "set version := \"" + [v.major, v.minor, v.point, v.buildNumber].join(".") + "\"";
}

p.setConfig({
    command: ["./sbt", getVersionString, "clean", "update", "compile", "test", "package"],
    dist: "target/scala_2.9.1",
    distInclude: "*.jar"
});

