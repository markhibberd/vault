var p = Ent.Project.create("vault", "external");
p.setVersion(1, 0, 0);


p.setConfig({
    command: ["make"],
    dist: "gen/dist",
    distInclude: "*.jar"
});
