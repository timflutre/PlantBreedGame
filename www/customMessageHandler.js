Shiny.addCustomMessageHandler("resetValue", function (variableName) {
  console.log("#########");
  console.log("rest: " + variableName);
  console.log("#########");
  Shiny.onInputChange(variableName, null, { priority: "event" });
});
