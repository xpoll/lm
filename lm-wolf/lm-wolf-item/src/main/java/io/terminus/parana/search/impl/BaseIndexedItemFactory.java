package io.terminus.parana.search.impl;

import com.google.common.collect.Lists;
import io.terminus.common.utils.BeanMapper;
import io.terminus.parana.attribute.dto.GroupedOtherAttribute;
import io.terminus.parana.attribute.dto.OtherAttribute;
import io.terminus.parana.cache.BackCategoryCacher;
import io.terminus.parana.category.impl.dao.ShopCategoryItemDao;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.category.model.ShopCategoryItem;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.search.dto.IndexedItem;
import io.terminus.parana.search.impl.IndexedItemFactory;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public abstract class BaseIndexedItemFactory implements IndexedItemFactory {
   private static final Logger log = LoggerFactory.getLogger(BaseIndexedItemFactory.class);
   protected final BackCategoryCacher backCategoryCacher;
   protected final ShopCategoryItemDao shopCategoryItemDao;
   protected final Class clazz;

   public BaseIndexedItemFactory(BackCategoryCacher backCategoryCacher, ShopCategoryItemDao shopCategoryItemDao) {
      this.backCategoryCacher = backCategoryCacher;
      this.shopCategoryItemDao = shopCategoryItemDao;
      Type genericSuperclass = this.getClass().getGenericSuperclass();
      if(genericSuperclass instanceof ParameterizedType) {
         this.clazz = (Class)((ParameterizedType)genericSuperclass).getActualTypeArguments()[0];
      } else {
         this.clazz = (Class)((ParameterizedType)this.getClass().getSuperclass().getGenericSuperclass()).getActualTypeArguments()[0];
      }

   }

   public IndexedItem create(Item item, ItemAttribute itemAttribute, Object... others) {
      T indexedItem = (IndexedItem)BeanMapper.map(item, this.clazz);
      Long categoryId = item.getCategoryId();
      List<BackCategory> backCategories = this.backCategoryCacher.findAncestorsOf(categoryId);
      List<Long> backCategoryIds = Lists.newArrayListWithCapacity(backCategories.size());

      for(BackCategory backCategory : backCategories) {
         backCategoryIds.add(backCategory.getId());
      }

      indexedItem.setCategoryIds(backCategoryIds);
      List<GroupedOtherAttribute> otherAttributes = itemAttribute.getOtherAttrs();
      if(!CollectionUtils.isEmpty(otherAttributes)) {
         List<String> attributes = Lists.newArrayList();

         for(GroupedOtherAttribute groupedOtherAttribute : otherAttributes) {
            for(OtherAttribute attr : groupedOtherAttribute.getOtherAttributes()) {
               attributes.add(attr.getAttrKey() + ":" + attr.getAttrVal());
            }
         }

         indexedItem.setAttributes(attributes);
      }

      List<ShopCategoryItem> shopCategoryItems = this.shopCategoryItemDao.findByShopIdAndItemId(item.getShopId(), item.getId());
      List<Long> shopCategoryIds = Lists.newArrayListWithCapacity(shopCategoryItems.size());

      for(ShopCategoryItem shopCategoryItem : shopCategoryItems) {
         shopCategoryIds.add(shopCategoryItem.getId());
      }

      indexedItem.setShopCategoryIds(shopCategoryIds);
      indexedItem.setPrice(item.getLowPrice());
      return indexedItem;
   }
}
