import "./main.css";
import {Elm} from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: {graphqlUrl: process.env.ELM_APP_GRAPHQL_URL}
});

registerServiceWorker();
