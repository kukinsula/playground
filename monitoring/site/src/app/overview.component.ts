import { Component, Input, OnInit, NgZone } from '@angular/core';
import { Observable } from 'rxjs/Rx';

import { MonitoringService } from './monitoring.service';
import { Overview } from './monitoring';

@Component({
  selector: 'overview',
  templateUrl: './overview.component.html',
})

export class OverviewComponent {
  title: string = 'Monitoring';
  overviewObs: Observable<Overview>;

  constructor(private service: MonitoringService) {
    this.overviewObs = this.service.getOverview();
  }
}
