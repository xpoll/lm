package cn.blmdz.wolf.search.impl.shop.impl;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Map;

import org.springframework.util.CollectionUtils;

import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.parana.search.dto.IndexedShop;
import cn.blmdz.wolf.parana.shop.model.Shop;

public abstract class BaseIndexedShopFactory<T extends IndexedShop>
  implements IndexedShopFactory<T>
{
  protected final Class<T> clazz;

  protected BaseIndexedShopFactory()
  {
    Type genericSuperclass = super.getClass().getGenericSuperclass();
    if (genericSuperclass instanceof ParameterizedType) {
      this.clazz = ((Class)((ParameterizedType)genericSuperclass).getActualTypeArguments()[0]);
    }
    else
      this.clazz = ((Class)((ParameterizedType)super.getClass().getSuperclass().getGenericSuperclass()).getActualTypeArguments()[0]);
  }

  public T create(Shop shop, Object[] others)
  {
    IndexedShop indexedShop = (IndexedShop)BeanMapper.map(shop, this.clazz);
    Map extra = shop.getExtra();
    if (!CollectionUtils.isEmpty(extra)) {
      if (extra.containsKey("contactPhone")) {
        indexedShop.setPhone((String)extra.get("contactPhone"));
      }
      fillAddresses((T) indexedShop, extra);
    }
    return (T) indexedShop;
  }

  private void fillAddresses(T indexedShop, Map<String, String> extra) {
    if (extra.containsKey("provinceId")) {
      indexedShop.setProvinceId(Integer.valueOf(Integer.parseInt((String)extra.get("provinceId"))));
    }
    if (extra.containsKey("province")) {
      indexedShop.setProvince((String)extra.get("province"));
    }
    if (extra.containsKey("cityId")) {
      indexedShop.setCityId(Integer.valueOf(Integer.parseInt((String)extra.get("cityId"))));
    }
    if (extra.containsKey("city")) {
      indexedShop.setCity((String)extra.get("city"));
    }
    if (extra.containsKey("regionId")) {
      indexedShop.setRegionId(Integer.valueOf(Integer.parseInt((String)extra.get("regionId"))));
    }
    if (extra.containsKey("region")) {
      indexedShop.setRegion((String)extra.get("region"));
    }
    if (extra.containsKey("street"))
      indexedShop.setStreet((String)extra.get("street"));
  }
}