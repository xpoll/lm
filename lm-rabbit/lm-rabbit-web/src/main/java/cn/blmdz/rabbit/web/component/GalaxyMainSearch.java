package cn.blmdz.rabbit.web.component;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.search.dto.SearchedItemInShopWithAggs;
import cn.blmdz.wolf.search.dto.SearchedItemWithAggs;
import cn.blmdz.wolf.search.dto.SearchedShopWithAggs;
import cn.blmdz.wolf.search.item.ItemSearchReadService;
import cn.blmdz.wolf.search.shop.ShopSearchReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * 政采云主搜
 *
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-02-19
 */
@Component
@Slf4j
public class GalaxyMainSearch {

    private final ItemSearchReadService itemSearchReadService;

    private final ShopSearchReadService shopSearchReadService;

    @Autowired
    public GalaxyMainSearch(ItemSearchReadService itemSearchReadService,
            ShopSearchReadService shopSearchReadService) {
        this.itemSearchReadService = itemSearchReadService;
        this.shopSearchReadService = shopSearchReadService;
    }

    /**
     * 搜索商品, 并且包括属性导航, 面包屑等
     *
     * @param pageNo       起始页码
     * @param pageSize     每页记录条数
     * @param params       搜索上下文
     * @return 搜索结果, 包括属性导航, 面包屑等
     */
    @Export(paramNames = {"pageNo", "pageSize", "params"})
    public Response<SearchedItemWithAggs> searchWithAggs(
            Integer pageNo, Integer pageSize, Map<String, String> params) {
        String templateName = "search.mustache";
        return itemSearchReadService.searchWithAggs(pageNo,pageSize, templateName, params);
    }

    @Export(paramNames = {"pageNo", "pageSize", "params"})
    public Response<SearchedItemInShopWithAggs> searchItemInShopWithAggs(
            Integer pageNo, Integer pageSize, Map<String, String> params) {
        String shopId = params.get("shopId");
        if (!StringUtils.hasText(shopId)) {
            log.error("shop id is required when search in shop");
            return Response.fail("shop.id.not.found");
        }
        //TODO 检查店铺是否存在和是否冻结?
        String templateName = "search.mustache";
        return itemSearchReadService.searchInShopWithAggs(pageNo, pageSize, templateName, params);
    }

    /**
     * 搜索店铺
     *
     * @param pageNo       起始页码
     * @param pageSize     每页记录条数
     * @param params       搜索上下文
     * @return 搜索结果, 包括属性导航, 面包屑等
     */
    @Export(paramNames = {"pageNo", "pageSize", "params"})
    public Response<SearchedShopWithAggs> searchedShopWithAggs(
            Integer pageNo, Integer pageSize, Map<String, String> params){
        String templateName = "search.mustache";
        return shopSearchReadService.searchWithAggs(pageNo, pageSize, templateName, params);
    }

}
