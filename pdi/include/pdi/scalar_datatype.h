/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#ifndef PDI_SCALAR_DATATYPE_H_
#define PDI_SCALAR_DATATYPE_H_

#include <functional>
#include <string>
#include <type_traits>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>

namespace PDI {

class PDI_EXPORT Scalar_datatype: public Datatype
{
	// Required to make_shared due to private ctor
	struct Shared_enabler;

	/// Size of the content in bytes or 0 if unknown
	size_t m_size;

	/// Size of the densified content in bytes
	size_t m_dense_size;

	/// Size of the alignment in bytes
	size_t m_align;

	/// Interpretation of the content
	Scalar_kind m_kind;

	/// copy function or null for memcpy
	std::function<void*(void*, const void*) > m_copy;

	/// destroy function or null for memcpy
	std::function<void(void*) > m_destroy;

	template <class T>
	static std::shared_ptr<Scalar_datatype> const cv_type_for_v;

	template <class T>
	static constexpr inline Scalar_kind kind_of()
	{
		if constexpr (std::is_integral<T>::value) {
			if constexpr (std::is_signed<T>::value) {
				return Scalar_kind::SIGNED;
			}
			return Scalar_kind::UNSIGNED;
		} else if constexpr (std::is_floating_point<T>::value) {
			return Scalar_kind::FLOAT;
		}
		return Scalar_kind::UNKNOWN;
	}

public:
	template <class T>
	static constexpr auto kind_of_v = kind_of<std::remove_cv_t<T>>();

	template <class T>
	static constexpr auto const & type_for_v = cv_type_for_v<std::remove_cv_t<T>>;

	/** Interpretation of the content
	 */
	Scalar_kind kind() const;

	Datatype_sptr densify() const override;

	Datatype_sptr evaluate(Context&) const override;

	bool dense() const override;

	size_t datasize() const override;

	size_t buffersize() const override;

	size_t alignment() const override;

	bool simple() const override;

	void* data_to_dense_copy(void* to, const void* from) const override;

	void* data_from_dense_copy(void* to, const void* from) const override;

	void destroy_data(void* ptr) const override;

	std::string debug_string() const override;

	bool operator== (const Datatype&) const override;

private:
	/** Creates new scalar datatype
	 *
	 * \param[in] kind kind of the scalar datatype
	 * \param[in] size buffersize of the scalar datatype
	 * \param[in] attributes attributes of the scalar datatype
	 */
	Scalar_datatype(Scalar_kind kind, size_t size, const Attributes_map& attributes = {});

	/** Creates new scalar datatype
	 *
	 * \param[in] kind kind of the scalar datatype
	 * \param[in] size buffersize of the scalar datatype
	 * \param[in] align alignment of the scalar datatype
	 * \param[in] attributes attributes of the scalar datatype
	 */
	Scalar_datatype(Scalar_kind kind, size_t size, size_t align, const Attributes_map& attributes = {});

	/** Creates new scalar datatype
	 *
	 * \param[in] kind kind of the scalar datatype
	 * \param[in] size buffersize of the scalar datatype
	 * \param[in] dense_size dense size of the scalar datatype
	 * \param[in] copy function that copies data of this datatype
	 * \param[in] destroy function that destroys data of this datatype (doesn't deallocate memory)
	 * \param[in] attributes attributes of the scalar datatype
	 */
	Scalar_datatype(
		Scalar_kind kind,
		size_t size,
		size_t align,
		size_t dense_size,
		std::function<void*(void*, const void*) > copy,
		std::function<void(void*) > destroy,
		const Attributes_map& attributes = {}
	);

public:
	/** Creates new scalar datatype
	 *
	 * \param[in] kind kind of the scalar datatype
	 * \param[in] size buffersize of the scalar datatype
	 * \param[in] attributes attributes of the scalar datatype
	 */
	static std::shared_ptr<Scalar_datatype> make(Scalar_kind kind, size_t size, const Attributes_map& attributes = {});

	/** Creates new scalar datatype
	 *
	 * \param[in] kind kind of the scalar datatype
	 * \param[in] size buffersize of the scalar datatype
	 * \param[in] align alignment of the scalar datatype
	 * \param[in] attributes attributes of the scalar datatype
	 */
	static std::shared_ptr<Scalar_datatype> make(Scalar_kind kind, size_t size, size_t align, const Attributes_map& attributes = {});

	/** Creates new scalar datatype
	 *
	 * \param[in] kind kind of the scalar datatype
	 * \param[in] size buffersize of the scalar datatype
	 * \param[in] align alignment of the scalar datatype
	 * \param[in] dense_size dense size of the scalar datatype
	 * \param[in] copy function that copies data of this datatype
	 * \param[in] destroy function that destroys data of this datatype (doesn't deallocate memory)
	 * \param[in] attributes attributes of the scalar datatype
	 */
	static std::shared_ptr<Scalar_datatype> make(
		Scalar_kind kind,
		size_t size,
		size_t align,
		size_t dense_size,
		std::function<void*(void*, const void*) > copy,
		std::function<void(void*) > destroy,
		const Attributes_map& attributes = {}
	);
};

const auto UNDEF_TYPE = Scalar_datatype::make(Scalar_kind::UNKNOWN, 0);

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint8_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint16_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint32_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint64_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int8_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int16_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int32_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int64_t>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<bool>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<float>;

extern template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<double>;

} // namespace PDI

#endif // PDI_SCALAR_DATATYPE_H_
