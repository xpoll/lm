package cn.blmdz.rabbit.item.impl.service;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.annotation.Nullable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Function;
import com.google.common.base.MoreObjects;
import com.google.common.base.Predicate;
import com.google.common.base.Splitter;
import com.google.common.base.Throwables;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.item.Enums.Size;
import cn.blmdz.rabbit.item.dto.SearchItemDto;
import cn.blmdz.rabbit.item.impl.dao.GalaxyItemDao;
import cn.blmdz.rabbit.item.service.GalaxyItemReadService;
import cn.blmdz.wolf.parana.attribute.dto.GroupedOtherAttribute;
import cn.blmdz.wolf.parana.attribute.dto.OtherAttribute;
import cn.blmdz.wolf.parana.attribute.dto.PreservedGroup;
import cn.blmdz.wolf.parana.attribute.dto.SkuAttribute;
import cn.blmdz.wolf.parana.cache.ItemCacher;
import cn.blmdz.wolf.parana.item.dto.FullItem;
import cn.blmdz.wolf.parana.item.dto.ItemWithAttribute;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.ItemAttribute;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.item.service.ItemReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 7/19/16
 * Time: 11:49 AM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@Service
public class GalaxyItemReadServiceImpl implements GalaxyItemReadService {

    private final GalaxyItemDao galaxyItemDao;
    private final ItemCacher itemCacher;
    private final ItemReadService itemReadService;

    @Autowired
    public GalaxyItemReadServiceImpl(GalaxyItemDao galaxyItemDao,
                                     ItemCacher itemCacher, ItemReadService itemReadService) {
        this.galaxyItemDao = galaxyItemDao;
        this.itemCacher = itemCacher;
        this.itemReadService = itemReadService;
    }

    @Override
    public Response<Long> countItemByShop(Long shopId) {
        try {
            Long count = galaxyItemDao.count(ImmutableMap.<String, Object>of("shopId", shopId));
            return Response.ok(count);
        } catch (Exception e) {
            log.error("fail to count item by shop:{}, cause:{}", shopId, Throwables.getStackTraceAsString(e));
            return Response.fail("item.count.by.shop.fail");
        }
    }

    @Override
    public Response<Item> findItemByIdWithCache(Long itemId) {
        try {
            if (itemId == null) {
                return Response.ok(null);
            }
            return Response.ok(itemCacher.findItemById(itemId));
        } catch (Exception e) {
            log.error("fail to find item by id:{}, cause:{}", itemId, Throwables.getStackTraceAsString(e));
            return Response.fail("item.find.fail");
        }
    }

    @Override
    public Response<Paging<SearchItemDto>> findSearchItemDtoByIds(String itemIds) {
        Paging<SearchItemDto> paging = new Paging<>();
        List<Long> itemIdList = Lists.transform(Splitter.on(",").splitToList(itemIds), new Function<String, Long>() {
            @Nullable
            @Override
            public Long apply(@Nullable String input) {
                return Long.valueOf(input);
            }
        });
        List<SearchItemDto> searchItemDtos = null;

        if(!itemIds.isEmpty()){
            searchItemDtos = Lists.transform(itemIdList, new Function<Long, SearchItemDto>() {
                @Nullable
                @Override
                public SearchItemDto apply(@Nullable Long input) {
                    SearchItemDto searchItemDto = new SearchItemDto();
                    Map<String, Integer> priceMap = Maps.newHashMap();
                    searchItemDto.setPrice(priceMap);
                    FullItem fullItem =
                            MoreObjects.firstNonNull(itemReadService.findFullInfoByItemId(input).getResult(), new FullItem());
                    Item item = MoreObjects.firstNonNull(fullItem.getItem(), new Item());
                    searchItemDto.setCompany(item.getShopName());
                    searchItemDto.setItemId(item.getId());
                    List<Sku> skus =
                            MoreObjects.firstNonNull(fullItem.getSkus(), Lists.<Sku>newArrayList());

                    for(final Size size : Size.values()){

                        Iterable<Sku> skuIterable = FluentIterable.from(skus).filter(new Predicate<Sku>() {
                            @Override
                            public boolean apply(@Nullable Sku input) {
                                List<SkuAttribute> skuAttributeList = input.getAttrs();
                                for(SkuAttribute skuAttribute : skuAttributeList){
                                    if(Objects.equals(skuAttribute.getAttrVal(), size.value())){
                                        return true;
                                    }
                                }
                                return false;
                            }
                        });

                        List<Sku> skuFilter = Lists.newArrayList(skuIterable);

                        if (!skuFilter.isEmpty()) {
                            Collections.sort(skuFilter, new Comparator<Sku>() {
                                @Override
                                public int compare(Sku o1, Sku o2) {
                                    if(o1.getPrice() > o2.getPrice()){
                                        return 1;
                                    }
                                    return -1;
                                }
                            });
                            priceMap.put(size.value(), skuFilter.get(0).getPrice());
                        }
                    }

                    ItemWithAttribute itemWithAttribute =
                            MoreObjects.firstNonNull(itemReadService.findItemWithAttributeById(input).getResult(), new ItemWithAttribute());

                    ItemAttribute itemAttribute =
                            MoreObjects.firstNonNull(itemWithAttribute.getItemAttribute(), new ItemAttribute());

                    List<GroupedOtherAttribute> groupedOtherAttributeList =
                            MoreObjects.firstNonNull(itemAttribute.getOtherAttrs(), Lists.<GroupedOtherAttribute>newArrayList());

                    Iterable<GroupedOtherAttribute> groupedOtherAttributeIterable =
                            FluentIterable.from(groupedOtherAttributeList).filter(new Predicate<GroupedOtherAttribute>() {
                                @Override
                                public boolean apply(@Nullable GroupedOtherAttribute input) {
                                    return Objects.equals(input.getGroup(), PreservedGroup.SERVICE.name());
                                }
                            });

                    List<GroupedOtherAttribute> filterGroupedOtherAttribute =
                            Lists.newArrayList(groupedOtherAttributeIterable);
                    if(!filterGroupedOtherAttribute.isEmpty()){
                        List<OtherAttribute> otherAttrs =
                                MoreObjects.firstNonNull(filterGroupedOtherAttribute.get(0).getOtherAttributes(), Lists.<OtherAttribute>newArrayList());

                        for(OtherAttribute otherAttrTemp : otherAttrs){
                            if(Objects.equals(otherAttrTemp.getAttrKey(), SearchItemDto.FROM)){
                                searchItemDto.setFrom(otherAttrTemp.getAttrVal());
                            }
                            if(Objects.equals(otherAttrTemp.getAttrKey(), SearchItemDto.TO)){
                                searchItemDto.setTo(otherAttrTemp.getAttrVal());
                            }
                            if(Objects.equals(otherAttrTemp.getAttrKey(), SearchItemDto.VALIDITY)){
                                searchItemDto.setValidity(otherAttrTemp.getAttrVal());
                            }
                        }
                    }

                    return searchItemDto;
                }
            });
        }

        searchItemDtos = MoreObjects.firstNonNull(searchItemDtos, Lists.<SearchItemDto>newArrayList());

        paging.setData(searchItemDtos);

        return Response.ok(paging);
    }

}
