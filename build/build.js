var p = Ent.Project.create("vault", "external");
p.setVersion(2, 0, 0);


p.setConfig({
    command: ["make"],
    dist: "gen/dist",
    distInclude: "*.jar"
});
