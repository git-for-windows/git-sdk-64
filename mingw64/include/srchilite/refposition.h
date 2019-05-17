#ifndef REFPOSITION_H
#define REFPOSITION_H

namespace srchilite {

/// where a reference must be put
enum RefPosition {
    NONE = 0,
    INLINE = 1, ///< put reference inline
    POSTLINE, ///< put all references of a line at the end of the line
    POSTDOC ///< put all references at the end of the document
};

}

#endif // REFPOSITION_H
