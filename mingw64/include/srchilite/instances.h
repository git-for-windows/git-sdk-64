/*
 * instances.h
 *
 *  Created on: Aug 21, 2009
 *      Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2009
 *  Copyright: See COPYING file that comes with this distribution
 */

#ifndef INSTANCES_H_
#define INSTANCES_H_

namespace srchilite {

class LangDefManager;
class LangMap;

/**
 * This class contains static and singleton instances for some
 * utility classes for LangDefManagers, LangMap, etc.
 *
 * Using this class ensures to use always the same instance for
 * doing these tasks, and if you change the global settings, through
 * Settings, you can reload() the files of these instances using the
 * new settings, thus enforcing consistency within a program using
 * this library.
 *
 * @since 3.1.1
 */
class Instances {
public:
    /**
     * @return the instance for LangDefManager
     */
    static LangDefManager *getLangDefManager();

    /**
     * @return the instance for LangMap for lang files
     */
    static LangMap *getLangMap();

    /**
     * @return the instance for LangMap for outlang files
     */
    static LangMap *getOutLangMap();

    /**
     * Reloads files (using the new settings) for each single instances.
     */
    static void reload();

    /**
     * Deletes all static instances.
     * This is not necessary, but if you're sure you're not using the library
     * anymore in your program, you can free some memory.
     */
    static void unload();
};

}

#endif /* INSTANCES_H_ */
