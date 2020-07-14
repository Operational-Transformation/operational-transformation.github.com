import React from "react";
import {createUseStyles} from 'react-jss'
import clsx from 'clsx';

const useStyles = createUseStyles({
  container: {
    position: "relative",
  },
  site: {
    background: "#eee",
    padding: "20px",
  },
  server: {
    width: "940px",
    height: "130px",
    position: "absolute",
    left: "0px",
    top: "0px",
  },
  client: {
    width: "460px",
    height: "230px",
  },
  alice: {
    position: "absolute",
    left: "0px",
    top: "280px",
  },
  bob: {
    position: "absolute",
    right: "0px",
    top: "280px",
  },
});

export const Visualization = () => {
  const classes = useStyles();

  return (
    <div className={classes.container}>
      <div className={clsx(classes.site, classes.server)}>
        <h2>Server</h2>
      </div>
      <div className={clsx(classes.site, classes.client, classes.alice)}>
        <h2>Alice</h2>
      </div>
      <div className={clsx(classes.site, classes.client, classes.bob)}>
        <h2>Bob</h2>
      </div>
    </div>
  );
};
