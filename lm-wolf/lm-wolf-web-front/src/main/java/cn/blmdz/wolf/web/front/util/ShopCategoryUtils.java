package cn.blmdz.wolf.web.front.util;

import java.util.List;
import java.util.Objects;

import javax.annotation.Nullable;

import cn.blmdz.wolf.common.utils.Iters;
import cn.blmdz.wolf.parana.category.dto.ShopCategoryWithChildren;
import cn.blmdz.wolf.parana.category.model.ShopCategory;

public class ShopCategoryUtils {
   public static boolean findShopCategoryAncestors(List<ShopCategoryWithChildren> nodes, Long id, List ancestors) {
      for(ShopCategoryWithChildren node : Iters.nullToEmpty(nodes)) {
         if(Objects.equals(id, node.getId())) {
            ancestors.add(node.getId());
            return true;
         }

         boolean mark = findShopCategoryAncestors(node.getChildren(), id, ancestors);
         if(mark) {
            ancestors.add(node.getId());
            return true;
         }
      }

      return false;
   }

   public static void findShopCategoryDescendants(List<ShopCategoryWithChildren> nodes, boolean flood, Long id, List descendants) {
      for(ShopCategoryWithChildren node : Iters.nullToEmpty(nodes)) {
         if(!flood && !Objects.equals(id, node.getId())) {
            findShopCategoryDescendants(node.getChildren(), false, id, descendants);
         } else {
            descendants.add(node.getId());
            findShopCategoryDescendants(node.getChildren(), true, id, descendants);
         }
      }

   }

   @Nullable
   public static ShopCategory fixName(List tree, Long shopCategoryId, Long shopId) {
      String name = fixNameHelper(tree, shopCategoryId, "");
      if(name == null) {
         return null;
      } else {
         ShopCategory sc = new ShopCategory();
         sc.setId(shopCategoryId);
         sc.setShopId(shopId);
         sc.setName(name);
         return sc;
      }
   }

   private static String fixNameHelper(List<ShopCategoryWithChildren> nodes, Long shopCategoryId, String pathName) {
      for(ShopCategoryWithChildren node : nodes) {
         String curName = pathName.length() == 0?node.getName():pathName + "/" + node.getName();
         if(Objects.equals(node.getId(), shopCategoryId)) {
            return curName;
         }

         String nextName = fixNameHelper(Iters.nullToEmpty(node.getChildren()), shopCategoryId, curName);
         if(nextName != null) {
            return nextName;
         }
      }

      return null;
   }
}
