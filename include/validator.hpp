#pragma once

#include "ast.hpp"
#include "common.hpp"

namespace cmm::ast {

// struct validation_visitor : public const_visitor<NODE_TYPES> {
//   void visit(const declaration::specifiers& specs) override {
//     REGISTER_INFO("validating specifiers");
//     bool typed = false;
//     for (const term::specifier* spec : specs) {
//       if (spec->type.is_type()) {
//         if (typed) {
//           throw too_many_types(specs.loc);
//         }
//
//         typed = true;
//       }
//     }
//
//     if (!typed) {
//       throw required_type(specs.loc);
//     }
//   }
// };
//
// struct validator {
//   STATIC_CLS(validator)
//   static void validate(const compound& comp) {
//     validation_visitor visitor;
//     comp.accept(visitor);
//   }
// };

} // namespace cmm::ast
