package cn.blmdz.wolf.parana.item.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.common.model.ParanaUser;

public interface ItemReadService {
   Response findById(Long var1);

   @Export(
      paramNames = {"itemIds"}
   )
   Response findByIds(List var1);

   Response findByItemCode(String var1);

   @Export(
      paramNames = {"user", "itemCode"}
   )
   Response findByShopIdAndCode(ParanaUser var1, String var2);

   @Export(
      paramNames = {"user", "itemCode", "itemId", "itemName", "status", "pageNo", "pageSize"}
   )
   Response findBy(ParanaUser var1, String var2, Long var3, String var4, Integer var5, Integer var6, Integer var7);

   @Export(
      paramNames = {"itemId"}
   )
   Response findItemWithAttributeById(Long var1);

   @Export(
      paramNames = {"itemId"}
   )
   Response findForView(Long var1);

   @Export(
      paramNames = {"itemId"}
   )
   Response findItemDetailInfoByItemId(Long var1);

   @Export(
      paramNames = {"itemId"}
   )
   Response findFullInfoByItemId(Long var1);

   @Export(
      paramNames = {"itemId"}
   )
   Response findRichTextById(Long var1);
}
