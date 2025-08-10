#pragma once

#include "allocator.hpp"
#include "types.hpp"

namespace cmm {

#define MAKE_ROW(CAT, PARENT, ...) \
  { \
    CAT, PARENT, { __VA_ARGS__ } \
  }

// Or for entries with no children:
#define MAKE_LEAF_ROW(CAT, PARENT) \
  { \
    CAT, PARENT, {} \
  }

namespace {
  constexpr type_metadata_store_t load_metadata() {
    using enum type_category_t;
    return {
        {MAKE_ROW(any_t, any_t, fundamental_t, compound_t),
         MAKE_ROW(fundamental_t, any_t, void_t, nullptr_t, arithmetic_t),
         MAKE_LEAF_ROW(void_t, fundamental_t),
         MAKE_LEAF_ROW(nullptr_t, fundamental_t),
         MAKE_ROW(arithmetic_t, fundamental_t, integral_t, float_t),
         MAKE_ROW(integral_t, arithmetic_t, bool_t, char_t, uint_t, sint_t),
         MAKE_LEAF_ROW(bool_t, integral_t),
         MAKE_LEAF_ROW(char_t, integral_t),
         MAKE_LEAF_ROW(uint_t, integral_t),
         MAKE_LEAF_ROW(sint_t, integral_t),
         MAKE_LEAF_ROW(float_t, arithmetic_t),
         MAKE_LEAF_ROW(compound_t, any_t),
         MAKE_ROW(indirection_t, compound_t, reference_t, pointer_t),
         MAKE_ROW(reference_t, indirection_t, lvalue_ref_t, rvalue_ref_t),
         MAKE_LEAF_ROW(lvalue_ref_t, reference_t),
         MAKE_LEAF_ROW(rvalue_ref_t, reference_t),
         MAKE_LEAF_ROW(pointer_t, indirection_t),
         MAKE_LEAF_ROW(array_t, compound_t),
         MAKE_LEAF_ROW(function_t, compound_t),
         MAKE_ROW(enum_t, compound_t, scoped_enum_t, unscoped_enum_t),
         MAKE_LEAF_ROW(scoped_enum_t, enum_t),
         MAKE_LEAF_ROW(unscoped_enum_t, enum_t),
         MAKE_LEAF_ROW(class_t, compound_t)}
    };
  };
} // namespace

template <typename... Args>
const type& type::create(type_category_t t, Args&&... args) {
  return *new (memory::Allocator::instance().allocate<type>()) type(t, std::forward<Args>(args)...);
}

inline constexpr const type_metadata_store_t type_metadata_store = load_metadata();

constexpr bool types::is_const_v::operator()(const type& t) { return t.c; }
constexpr bool types::is_indirect_v::operator()(cr_type t) {
  bool value = types::belongs_to(t.category, type_category_t::indirection_t);
  if (value) {
    ASSERT(t.underlying != nullptr);
  }
  return value;
}
constexpr bool types::is_reference_v::operator()(cr_type t) {
  return types::belongs_to(t.category, type_category_t::reference_t);
}

} // namespace cmm
