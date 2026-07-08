(function() {
  if (typeof Shiny === "undefined") {
    return;
  }
  Shiny.addCustomMessageHandler("liberties_job_push", function(msg) {
    if (!msg || typeof msg.rev === "undefined") {
      return;
    }
    Shiny.setInputValue("job_push_rev", msg.rev, {priority: "event"});
    if (typeof msg.sig !== "undefined") {
      Shiny.setInputValue("job_push_sig", msg.sig, {priority: "event"});
    }
  });
})();
