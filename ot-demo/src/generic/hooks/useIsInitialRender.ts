import { useEffect, useState } from "react";

export const useIsInitialRender = () => {
  const [isInitialRender, setIsInitialRender] = useState<boolean>(true);

  useEffect(() => {
    const timeout = setTimeout(() => {
      setIsInitialRender(false);
    }, 10);
    return () => {
      clearTimeout(timeout);
    };
  }, []);

  return isInitialRender;
};
