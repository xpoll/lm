package cn.blmdz.wolf.search.impl;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.attribute.dto.GroupedOtherAttribute;
import cn.blmdz.wolf.attribute.dto.OtherAttribute;
import cn.blmdz.wolf.cache.BackCategoryCacher;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryItemDao;
import cn.blmdz.wolf.category.model.BackCategory;
import cn.blmdz.wolf.category.model.ShopCategoryItem;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.ItemAttribute;
import cn.blmdz.wolf.search.dto.IndexedItem;

public abstract class BaseIndexedItemFactory<T extends IndexedItem> implements IndexedItemFactory<IndexedItem> {
	private static final Logger log = LoggerFactory.getLogger(BaseIndexedItemFactory.class);
	protected final BackCategoryCacher backCategoryCacher;
	protected final ShopCategoryItemDao shopCategoryItemDao;
	protected final Class<T> clazz;

	public BaseIndexedItemFactory(BackCategoryCacher backCategoryCacher, ShopCategoryItemDao shopCategoryItemDao) {
		this.backCategoryCacher = backCategoryCacher;
		this.shopCategoryItemDao = shopCategoryItemDao;
		Type genericSuperclass = super.getClass().getGenericSuperclass();
		if (genericSuperclass instanceof ParameterizedType) {
			this.clazz = ((Class) ((ParameterizedType) genericSuperclass).getActualTypeArguments()[0]);
		} else
			this.clazz = ((Class) ((ParameterizedType) super.getClass().getSuperclass().getGenericSuperclass())
					.getActualTypeArguments()[0]);
	}

	public IndexedItem create(Item item, ItemAttribute itemAttribute, Object[] others) {
		IndexedItem indexedItem = (IndexedItem) BeanMapper.map(item, this.clazz);

		Long categoryId = item.getCategoryId();
		List<BackCategory> backCategories = this.backCategoryCacher.findAncestorsOf(categoryId);
		List backCategoryIds = Lists.newArrayListWithCapacity(backCategories.size());
		for (BackCategory backCategory : backCategories) {
			backCategoryIds.add(backCategory.getId());
		}

		indexedItem.setCategoryIds(backCategoryIds);

		List<GroupedOtherAttribute> otherAttributes = itemAttribute.getOtherAttrs();

		if (!CollectionUtils.isEmpty(otherAttributes)) {
			List attributes = Lists.newArrayList();
			for (GroupedOtherAttribute groupedOtherAttribute : otherAttributes) {
				for (OtherAttribute attr : groupedOtherAttribute.getOtherAttributes()) {
					attributes.add(attr.getAttrKey() + ":" + attr.getAttrVal());
				}
			}
			indexedItem.setAttributes(attributes);
		}
		List<ShopCategoryItem> shopCategoryItems = this.shopCategoryItemDao.findByShopIdAndItemId(item.getShopId(),
				item.getId());
		List shopCategoryIds = Lists.newArrayListWithCapacity(shopCategoryItems.size());
		for (ShopCategoryItem shopCategoryItem : shopCategoryItems) {
			shopCategoryIds.add(shopCategoryItem.getId());
		}
		indexedItem.setShopCategoryIds(shopCategoryIds);

		indexedItem.setPrice(item.getLowPrice());

		return  indexedItem;
	}
}