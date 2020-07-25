import { createUseStyles } from "react-jss";

export const useSharedStyles = createUseStyles({
  site: {
    background: "#eee",
    padding: "20px",
    margin: "0 10px",
    "& h2": {
      margin: "0 0 16px",
      "& svg": {
        margin: "0 12px 0 4px",
        verticalAlign: "bottom",
      },
    },
  },
});
