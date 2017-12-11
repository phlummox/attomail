
{-# LANGUAGE CPP #-}
-- -pgmP options needed because the cpp built in to ghc
-- doesn't support stringification.
{-# OPTIONS_GHC -pgmP cpp #-}

{- |

Location of config file. @\/etc\/attomail.conf@ by default,
overridable at compile time - see the README for details.
-}

module ConfigLocation where

#define STRINGIFY(x) #x
#define STRINGIFY2(x) STRINGIFY(x)
#define CP STRINGIFY2(CONF_PATH)

-- | Location of the config file, baked in at compile time.
configFileLocn :: String
configFileLocn =
#ifdef CONF_PATH
    CP    
#else
    "/etc/attomail.conf"
#endif

