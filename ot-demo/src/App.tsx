import React from 'react';
import { Visualization } from './Visualization';
import {createUseStyles} from 'react-jss';

const useStyles = createUseStyles({
  wrapper: {
    width: "940px",
    margin: "20px auto",
  },
  header: {
    margin: "0 0 40px",
    textAlign: "center",
  },
});

function App() {
  const classes = useStyles();

  return (
    <div className={classes.wrapper}>
      <h1 className={classes.header}>Visualization of OT with a central server</h1>
      <Visualization />
    </div>
  );
}

export default App;
