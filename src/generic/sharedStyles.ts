import { createUseStyles } from "react-jss";
import { ClientName } from "./types/operation";

export const useSharedStyles = createUseStyles({
  site: {
    background: "#eee",
    padding: "20px",
    margin: "0 15px",
    "& h2": {
      margin: "0 0 16px",
      "& svg": {
        margin: "0 12px 0 4px",
        verticalAlign: "bottom",
      },
    },
  },
});

export const getClientColor = (clientName: ClientName): string => {
  switch (clientName) {
    case ClientName.Alice:
      return "#0074D9";
    case ClientName.Bob:
      return "#e2451e";
  }
};
