/*
 * settings.h
 *
 *  Created on: Apr 18, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2008
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef SETTINGS_H_
#define SETTINGS_H_

#include <string>

namespace srchilite {

/// an error dealing with setting configuration file
enum SettingError {
    NO_SETTING_ERROR = 0, CANT_CREATE_DIR, CANT_CREATE_FILE
};

/**
 * Handles the settings for source-highlight (and its library).
 * At the moment the only setting that is stored is the value for datadir
 * (command line option --data-dir), that specifies whether source-highlight
 * should search for .lang, .outlang, .map and other files.
 *
 * A default value for this datadir is hardcoded, based on the configure
 * --prefix option: prefix/share/source-highlight (where prefix is the
 * one specified at configure, and it defaults to /usr/local).
 *
 * When an object of this class is created, it will get the value
 * of the environment variable $HOME.
 *
 * The method checkForConfFile() should be used to check whether configuration
 * exists at all.
 *
 * The method readDataDir() can be used to retrieve the datadir value
 * from the configuration file.  If it returns false it can be assumed
 * that no configuration file was found.
 *
 * The method checkForTestFile() should be used to make sure that datadir
 * contains the files needed by source-highlight.  If it returns false
 * it can be assumed that that directory is not correct (that directory
 * may not exist at all).
 *
 * The new value for datadir can be set with method setDataDir().
 *
 * The method save() will save the current value of datadir in the
 * configuration file (it also creates the directory if it does not exist).
 *
 * This class also provides a static method retrieveDataDir() which uses
 * an object of this class to retrieve datadir (if the environment
 * variable SOURCE_HIGHLIGHT_DATADIR is not set).
 * If also the reading of configuration file fails, then it returns the
 * hardcoded value.
 *
 * Thus, the users of the
 * library should always rely on this static method for retrieving
 * the datadir value.  The other methods of this class should be used
 * when (possibly) configuring the library from within the program itself.
 * For instance this is done by the program source-highlight-settings.
 *
 * An alternative use, is to set a global data dir value with
 * setGlobalDataDir(); this way, retrieveDataDir() will always return
 * the global value (this enforces consistency in a library using
 * the source-highlight library).
 */
class Settings {
    /// the home directory of the user
    std::string homeDir;

    /// the directory for configuration file (default: $HOME/.source-highlight/)
    std::string confDir;

    /// the name of the configuration file (default: source-highlight.conf)
    std::string confFileName;

    /// test file to search for in the datadir (default: lang.map)
    std::string testFileName;

    /// the data dir (used for .lang, .outlang, lang.map, etc), this is read from the configuration file
    std::string dataDir;
public:
    Settings();
    ~Settings();

    const std::string getConfDir() const {
        return confDir;
    }

    void setConfDir(const std::string &dir) {
        confDir = dir;
    }

    const std::string getConfFileName() const {
        return confFileName;
    }

    const std::string getTestFileName() const {
        return testFileName;
    }

    void setTestFileName(const std::string &name) {
        testFileName = name;
    }

    const std::string getDataDir() const {
        return dataDir;
    }

    void setDataDir(const std::string &ddir) {
        dataDir = ddir;
    }

    /**
     * Checks whether the conf file exists
     * @return true if the conf file exists
     */
    bool checkForConfFile();

    /**
     * Checks whether the test file is in the datadir
     * @return true if the test file is in the datadir
     */
    bool checkForTestFile();

    /**
     * Reads the datadir from the configuration file
     * @return true if the datadir was specified in the configuration file
     */
    bool readDataDir();

    /**
     * Saves the setting (for datadir) in the conf file
     * @return a possible error in case saving (or creating directory) fails
     */
    SettingError save();

    /**
     * Retrieves the value for the data dir.
     *
     * If the global value was set with setGlobalDataDir() then always returns this
     * global value.  It returns the value of the environment
     * variable SOURCE_HIGHLIGHT_DATADIR if set.  Otherwise, it returns the
     * value read from the configuration file.
     *
     * Thus, the users of the library should always rely on this static method for retrieving
     * the datadir value.
     *
     * If also the reading of configuration file fails, then it returns the
     * hardcoded value.
     *
     * If the global data dir was set (and it's not empty) with setGlobalDataDir, then this
     * method will always return the global value, without inspecting the environment
     * variable nor the configuration file.
     *
     * @param reload whether to perform the retrieval from scratch (otherwise it is
     * cached)
     * @return the value for datadir
     */
    static const std::string retrieveDataDir(bool reload = false);

    /**
     * @return the hardcoded datadir value
     */
    static const std::string getDefaultDataDir();

    /**
     * Sets the global data dir value.  If the passed value is not empty,
     * then retrieveDataDir() will always return this value (and it won't
     * read the configuration file).
     * @param dataDir
     */
    static void setGlobalDataDir(const std::string &dataDir);

    /**
     * Checks whether the current retrieved data dir is a valid
     * data dir value for source-highlight
     * @return whether the current retrieved data dir is a valid
     * data dir value for source-highlight.
     */
    static bool checkSettings();
};

}

#endif /* SETTINGS_H_ */
