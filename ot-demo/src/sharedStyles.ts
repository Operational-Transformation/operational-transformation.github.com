import {createUseStyles} from "react-jss";

export const useSharedStyles = createUseStyles({
    site: {
        background: "#eee",
        padding: "20px",
        "& h2": {
            margin: "0 0 16px",
        },
    },
});