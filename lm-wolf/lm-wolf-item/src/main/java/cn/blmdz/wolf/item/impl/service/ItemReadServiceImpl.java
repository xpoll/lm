package cn.blmdz.wolf.item.impl.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.google.common.base.Supplier;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Table;
import com.google.common.collect.Tables;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.item.impl.dao.ItemAttributeDao;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.item.impl.dao.ItemDetailDao;
import cn.blmdz.wolf.item.impl.dao.SkuDao;
import cn.blmdz.wolf.parana.attribute.dto.GroupedSkuAttribute;
import cn.blmdz.wolf.parana.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.dto.ItemWithAttribute;
import cn.blmdz.wolf.parana.item.dto.ViewedItem;
import cn.blmdz.wolf.parana.item.dto.ViewedItemDetailInfo;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.item.model.ItemDetail;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.item.service.ItemReadService;

@Service
public class ItemReadServiceImpl implements ItemReadService {
   private static final Logger log = LoggerFactory.getLogger(ItemReadServiceImpl.class);
   private final ItemDao itemDao;
   private final ItemAttributeDao itemAttributeDao;
   private final ItemDetailDao itemDetailDao;
   private final SkuDao skuDao;

   @Autowired
   public ItemReadServiceImpl(ItemDao itemDao, ItemAttributeDao itemAttributeDao, ItemDetailDao itemDetailDao, SkuDao skuDao) {
      this.itemDao = itemDao;
      this.itemAttributeDao = itemAttributeDao;
      this.itemDetailDao = itemDetailDao;
      this.skuDao = skuDao;
   }

   public Response findById(Long itemId) {
      try {
         Item item = (Item)this.itemDao.findById(itemId);
         if(item == null) {
            log.error("item(id={}) not found", itemId);
            return Response.fail("item.not.found");
         } else {
            return Response.ok(item);
         }
      } catch (Exception var3) {
         log.error("failed to find item(id={}), cause:{}", itemId, Throwables.getStackTraceAsString(var3));
         return Response.fail("item.find.fail");
      }
   }

   public Response findByIds(List itemIds) {
      if(CollectionUtils.isEmpty(itemIds)) {
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<Item> items = this.itemDao.findByIds(itemIds);
            return Response.ok(items);
         } catch (Exception var3) {
            log.error("failed to find items(ids={}), cause:{}", itemIds, Throwables.getStackTraceAsString(var3));
            return Response.fail("item.find.fail");
         }
      }
   }

   public Response findByItemCode(String itemCode) {
      try {
         List<Item> items = this.itemDao.findByItemCode(itemCode);
         return Response.ok(items);
      } catch (Exception var3) {
         log.error("failed to find item(itemCode={}), cause:{}", itemCode, Throwables.getStackTraceAsString(var3));
         return Response.fail("item.find.fail");
      }
   }

   public Response findByShopIdAndCode(ParanaUser user, String itemCode) {
      try {
         List<Item> items = this.itemDao.findByShopIdAndCode(user.getShopId(), itemCode);
         return Response.ok(items);
      } catch (Exception var4) {
         log.error("failed to find items by shopId={} and itemCode={}, cause:{}", new Object[]{user.getShopId(), itemCode, Throwables.getStackTraceAsString(var4)});
         return Response.fail("item.find.fail");
      }
   }

   public Response findBy(ParanaUser user, String itemCode, Long itemId, String itemName, Integer status, Integer pageNo, Integer pageSize) {
      try {
         PageInfo page = new PageInfo(pageNo, pageSize);
         Map<String, Object> criteria = Maps.newHashMap();
         this.fillDefaultOrdinaryItemTypes(criteria);
         criteria.put("shopId", user.getShopId());
         criteria.put("offset", page.getOffset());
         criteria.put("limit", page.getLimit());
         if(status != null) {
            criteria.put("status", status);
         }

         if(StringUtils.hasText(itemCode)) {
            criteria.put("itemCode", itemCode);
         }

         if(itemId != null) {
            criteria.put("ids", ImmutableList.of(itemId));
         }

         if(StringUtils.hasText(itemName)) {
            criteria.put("name", itemName);
         }

         return Response.ok(this.itemDao.paging(criteria));
      } catch (Exception var10) {
         log.error("failed to find items for seller(shopId={}), pageNo={}, pageSize={}, status={}, cause: {}", new Object[]{user.getShopId(), pageNo, pageSize, status, Throwables.getStackTraceAsString(var10)});
         return Response.fail("item.find.fail");
      }
   }

   private void fillDefaultOrdinaryItemTypes(Map criteria) {
      criteria.put("sortBy", "id");
      criteria.put("sortType", Integer.valueOf(2));
   }

   public Response<ItemWithAttribute> findItemWithAttributeById(Long itemId) {
      try {
         ItemWithAttribute itemWithAttribute = new ItemWithAttribute();
         ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
         itemWithAttribute.setItemAttribute(itemAttribute);
         return Response.ok(itemWithAttribute);
      } catch (Exception var4) {
         log.error("failed to find item with attribute for item(id={}),cause:{}", itemId, Throwables.getStackTraceAsString(var4));
         return Response.fail("item.find.fail");
      }
   }

   public Response findForView(Long itemId) {
      Response<Item> rItem = this.findById(itemId);
      if(!rItem.isSuccess()) {
         return Response.fail(rItem.getError());
      } else {
         Item item = (Item)rItem.getResult();
         ItemDetail itemDetail = this.itemDetailDao.findByItemId(itemId);
         if(itemDetail == null) {
            log.error("no item detail found for item(id={})", itemId);
            return Response.fail("item.detail.not.found");
         } else {
            ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
            if(itemAttribute == null) {
               log.error("no item attribute found for item(id={})", itemId);
               return Response.fail("item.attribute.not.found");
            } else {
               List<Sku> skus = this.skuDao.findByItemId(itemId);
               if(CollectionUtils.isEmpty(skus)) {
                  log.error("no sku found for item(id={})", itemId);
                  return Response.fail("item.sku.not.found");
               } else {
                  ViewedItem viewedItem = new ViewedItem();
                  viewedItem.setItem(item);
                  viewedItem.setImageInfos(itemDetail.getImages());
                  viewedItem.setSkus(skus);
                  viewedItem.setGroupedSkuAttrs(this.groupSkuAttrs(skus, itemAttribute.getSkuAttrs()));
                  return Response.ok(viewedItem);
               }
            }
         }
      }
   }

   public Response findItemDetailInfoByItemId(Long itemId) {
      try {
         ItemDetail itemDetail = this.itemDetailDao.findByItemId(itemId);
         if(itemDetail == null) {
            log.error("no item detail found for item (id={})", itemId);
            return Response.fail("item.detail.not.found");
         } else {
            ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
            if(itemAttribute == null) {
               log.error("no item attribute found for item (id={})", itemId);
               return Response.fail("item.attribute.not.found");
            } else {
               ViewedItemDetailInfo viewedItemDetailInfo = new ViewedItemDetailInfo();
               viewedItemDetailInfo.setItemDetail(itemDetail);
               viewedItemDetailInfo.setGroupedOtherAttributes(itemAttribute.getOtherAttrs());
               return Response.ok(viewedItemDetailInfo);
            }
         }
      } catch (Exception var5) {
         log.error("failed to find ViewedItemDetailInfo for item(id={}), cause:{}", itemId, Throwables.getStackTraceAsString(var5));
         return Response.fail("item.detail.find.fail");
      }
   }

   private List groupSkuAttrs(List<Sku> skus, List<GroupedSkuAttribute> groupedSkuAttributes) {
      if(skus.isEmpty()) {
         return Collections.emptyList();
      } else if(CollectionUtils.isEmpty(groupedSkuAttributes)) {
         return Collections.emptyList();
      } else {
         Table<String, String, SkuAttribute> byKeyVal = Tables.newCustomTable(Maps.<String, Map<String, SkuAttribute>>newLinkedHashMap(), new Supplier<Map<String, SkuAttribute>>() {
            public Map<String, SkuAttribute> get() {
               return Maps.newLinkedHashMap();
            }
         });

         for(GroupedSkuAttribute skuAttr : groupedSkuAttributes) {
            String attrKey = skuAttr.getAttrKey();
            if(!CollectionUtils.isEmpty(skuAttr.getSkuAttributes())) {
               for(SkuAttribute skuAttribute : skuAttr.getSkuAttributes()) {
                  byKeyVal.put(attrKey, skuAttribute.getAttrVal(), skuAttribute);
               }
            }
         }

         Multimap<String, SkuAttribute> attrKeyToAttrs = LinkedHashMultimap.create();

         for(Sku sku : skus) {
            if(!CollectionUtils.isEmpty(sku.getAttrs())) {
               for(SkuAttribute skuAttribute : sku.getAttrs()) {
                  String attrKey = skuAttribute.getAttrKey();
                  String attrVal = skuAttribute.getAttrVal();
                  attrKeyToAttrs.put(attrKey, byKeyVal.get(attrKey, attrVal));
               }
            }
         }

         List<GroupedSkuAttribute> groupedSkuAttrs = Lists.newArrayList();

         for(String attrKey : attrKeyToAttrs.keySet()) {
            GroupedSkuAttribute gsa = new GroupedSkuAttribute();
            gsa.setAttrKey(attrKey);
            gsa.setSkuAttributes(Lists.newArrayList(attrKeyToAttrs.get(attrKey)));
            groupedSkuAttrs.add(gsa);
         }

         return groupedSkuAttrs;
      }
   }

   public Response<FullItem> findFullInfoByItemId(Long itemId) {
      try {
         Item item = (Item)this.itemDao.findById(itemId);
         if(item == null) {
            log.error("item(id={}) not found", itemId);
            return Response.fail("item.not.found");
         } else {
            ItemDetail itemDetail = this.itemDetailDao.findByItemId(itemId);
            ItemAttribute itemAttribute = this.itemAttributeDao.findByItemId(itemId);
            List<Sku> skus = this.skuDao.findByItemId(itemId);
            FullItem fullItem = new FullItem();
            fullItem.setItem(item);
            fullItem.setItemDetail(itemDetail);
            fullItem.setSkus(skus);
            fullItem.setGroupedOtherAttributes(itemAttribute.getOtherAttrs());
            fullItem.setGroupedSkuAttributes(itemAttribute.getSkuAttrs());
            return Response.ok(fullItem);
         }
      } catch (Exception var7) {
         log.error("failed to find full item info by itemId={}, cause:{}", itemId, Throwables.getStackTraceAsString(var7));
         return Response.fail("item.info.find.fail");
      }
   }

   public Response findRichTextById(Long itemId) {
      try {
         ItemDetail itemDetail = this.itemDetailDao.findByItemId(itemId);
         if(itemDetail == null) {
            log.error("item detail(itemId={}) not found", itemId);
            return Response.fail("item.detail.not.found");
         } else {
            return Response.ok(itemDetail.getDetail());
         }
      } catch (Exception var3) {
         log.error("failed to find item detail for item(id={}),cause:{}", itemId, Throwables.getStackTraceAsString(var3));
         return Response.fail("item.detail.find.fail");
      }
   }
}
