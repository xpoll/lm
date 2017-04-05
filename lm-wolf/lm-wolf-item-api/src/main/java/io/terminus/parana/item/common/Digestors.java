package io.terminus.parana.item.common;

import com.google.common.base.Charsets;
import com.google.common.base.MoreObjects;
import com.google.common.hash.HashFunction;
import com.google.common.hash.Hashing;
import io.terminus.parana.item.model.Item;
import io.terminus.parana.item.model.ItemAttribute;
import io.terminus.parana.item.model.ItemDetail;
import io.terminus.parana.spu.model.Spu;
import io.terminus.parana.spu.model.SpuAttribute;
import io.terminus.parana.spu.model.SpuDetail;

public final class Digestors {
   private static final HashFunction md5 = Hashing.md5();

   public static String itemDigest(Item item, ItemDetail itemDetail, ItemAttribute itemAttribute) {
      return md5.newHasher().putString((CharSequence)MoreObjects.firstNonNull(item.getItemCode(), ""), Charsets.UTF_8).putLong(item.getShopId().longValue()).putString((CharSequence)MoreObjects.firstNonNull(item.getShopName(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(item.getName(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(item.getMainImage(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(itemDetail.getImagesJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(item.getAdvertise(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(item.getSpecification(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(item.getExtraJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(item.getTagsJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(itemAttribute.getSkuAttrsJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(itemAttribute.getOtherAttrsJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(itemDetail.getDetail(), ""), Charsets.UTF_8).hash().toString();
   }

   public static String spuDigest(Spu spu, SpuDetail spuDetail, SpuAttribute spuAttribute) {
      return md5.newHasher().putString((CharSequence)MoreObjects.firstNonNull(spu.getSpuCode(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spu.getName(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spu.getMainImage(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spuDetail.getImagesJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spu.getAdvertise(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spu.getSpecification(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spu.getExtraJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spuAttribute.getSkuAttrsJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spuAttribute.getOtherAttrsJson(), ""), Charsets.UTF_8).putString((CharSequence)MoreObjects.firstNonNull(spuDetail.getDetail(), ""), Charsets.UTF_8).hash().toString();
   }
}
