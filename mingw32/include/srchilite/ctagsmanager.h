//
// Author: Lorenzo Bettini <http://www.lorenzobettini.it>, (C) 2004-2008
//
// Copyright: See COPYING file that comes with this distribution
//

#ifndef CTAGSMANAGER_H_
#define CTAGSMANAGER_H_

#include <string>

#include "refposition.h"
#include "textstyles.h"

namespace srchilite {

class CTagsCollector;
class CTagsFormatter;

/**
 * Takes care of running ctags and to generate a CTagsFormmatter
 */
class CTagsManager {
    /// the ctags file name
    std::string ctagsFile;

    /// the possible ctags command to execute
    std::string ctagsCmd;

    /// whether to run ctags command
    bool runCTags;

    /// the position for generated references
    RefPosition refPosition;

    /// the shared instance shared by all the created CTagsFormatters
    CTagsCollector *ctagsCollector;
public:
    /**
     * @param _ctagsFile the ctags file name
     * @param _ctagsCmd the ctags line command
     * @param _runCTags whether to run the ctags command
     * @param _refPosition the position for generated references
     */
    CTagsManager(const std::string &_ctagsFile, const std::string &_ctagsCmd,
            bool _runCTags, RefPosition _refPosition);

    ~CTagsManager();

    /**
     * Runs the ctags program.
     * @throws IOException if the run fails
     */
    void runCTagsCmd();

    /**
     * Creates a CTagsFormatter (if required, it previously run the ctags
     * command).
     * @param r the style for references and anchors
     * @throws IOException if running the ctags command fails
     */
    CTagsFormatter *createCTagsFormatter(const TextStyles::RefTextStyle &r);
};

}

#endif /*CTAGSMANAGER_H_*/
