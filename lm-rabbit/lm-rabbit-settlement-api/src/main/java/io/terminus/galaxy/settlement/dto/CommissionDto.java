package io.terminus.galaxy.settlement.dto;

import io.terminus.parana.settlement.model.CommissionDetail;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * Created with IntelliJ IDEA
 * Author: songrenfei
 * Date: 3/24/16
 * Time: 6:38 PM
 */
@Data
public class CommissionDto implements Serializable{


    private static final long serialVersionUID = -1206633029484111672L;

    /**
     * 佣金
     */
    private Long commisson;

    /**
     * 佣金明细
     */
    private List<CommissionDetail> commissionDetails;
}
